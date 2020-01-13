{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.SignUp where

import           Import

import           Form.Auth.SignUp
import           Local.Persist.UserRole
import           Settings.MailRu               ( password, serverName, smtpPort,
                                                 usernameFenixNoreply,
                                                 usernameOutbirdsNoreply )
import           Type.Auth.SignUp              ( SignUpFormData (..) )

import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           Network.HaskellNet.SMTP
import           Network.HaskellNet.SMTP.SSL
import           Text.Blaze.Html.Renderer.Text ( renderHtml )
import           Yesod.Auth.Util.PasswordStore ( makePassword )

authType :: AuthType
authType = PLAIN

fenixFrom :: String
fenixFrom = "noreply@fenix.info"

outbirdsFrom :: String
outbirdsFrom = "noreply@outb.info"

subject :: String
subject = "Подтвердите ваш электронный ящик"


getSignUpR :: Handler Html
getSignUpR = do
    mUser <- maybeAuthId
    when (isJust mUser) $ redirect HomeR
    (widget, enctype) <- generateFormPost signUpForm
    defaultLayout $ do
        let mayError = Nothing :: Maybe Text
        setAppPageTitle MsgSignUpPageTitle
        $(widgetFile "auth/signup")


postSignUpR :: Handler Html
postSignUpR = do
    ((signUpDataResult, widget), enctype) <- runFormPost signUpForm
    case signUpDataResult of
        FormSuccess (SignUpFormData email pass conf) -> if pass == conf
            then do
                key          <- appNonce128urlT
                createResult <- createUniqueLogin email pass key
                case createResult of
                    CreateError e -> do
                        let mayError = Just e
                        defaultLayout $(widgetFile "auth/signup")
                    CreateSuccess -> do
                        cleanUpRefCookie
                        sendEmailActivationMessage email key
                        defaultLayout $(widgetFile "auth/verify-message")
            else defaultLayout $(widgetFile "auth/verify-message")
        _ -> do
            let mayError = Nothing :: Maybe Text
            defaultLayout $(widgetFile "auth/signup")
  where

    createUniqueLogin :: Text -> Text -> Text -> Handler UserCreateResult
    createUniqueLogin login pass key = do
        ref <- maybeReferrer
        token <- appNonce128urlT
        runDB $ do
            mayExisting <- getBy $ UniqueEmail login
            case mayExisting of
                Just _ -> return $ CreateError
                        "Пользователь с таким эл.адресом уже существует"
                Nothing -> do
                    saltedPass <- liftIO $
                            decodeUtf8 <$> makePassword (encodeUtf8 pass) 14
                    let newUser = User login (Just saltedPass) Client
                    userId <- insert newUser
                    let newEmail = Email login (Just userId) (Just key)
                    _ <- insert newEmail
                    _ <- case ref of
                        Nothing -> return []
                        Just r  -> (:[]) <$> addReferral r userId
                    let refTok = Referrer userId token
                    _ <- insert refTok
                    return CreateSuccess

    sendEmailActivationMessage :: Text -> Text -> Handler ()
    sendEmailActivationMessage email key = do
        urlRender <- getUrlRender
        let verUrl = urlRender $ SignUpVerifyR email key
        projType <- appType . appSettings <$> getYesod
        let (from, exName, exHost) = if projType == FenixApp
                then (usernameFenixNoreply, "FENIX.TRADING", "FENIX.TRADING")
                else (usernameOutbirdsNoreply, "OutBirds", "OUTB.INFO")
        liftIO $ do
            conn <- connectSMTPSSLWithSettings
                (unpack serverName)
                (defaultSettingsSMTPSSL { sslPort = smtpPort })
            authSuccess <-
                Network.HaskellNet.SMTP.SSL.authenticate
                    PLAIN
                    (unpack usernameOutbirdsNoreply)
                    (unpack password)
                    conn
            if authSuccess
                then sendMimeMail (T.unpack email)
                                  (unpack from)
                                  subject
                                  (plainBody email verUrl exName exHost)
                                  (htmlBody email verUrl exName exHost)
                                  []
                                  conn
                else putStrLn "Authentication failed."
            closeSMTP conn

    plainBody = textContent

    htmlBody = htmlContent


data UserCreateResult
    = CreateError Text
    | CreateSuccess

textContent :: Text -> Text -> Text -> Text -> TL.Text
textContent _email url exchangerName exchangerHost = renderHtml [shamlet|
    Необходимо подтверждение электронной почты.

    Для завершения регистрации на сайте #{exchangerHost} пройдите по ссылке:
    #{url}

    Если это письмо пришло Вам по ошибке, просто проигнорируйте его.

    ----------------------------------------------------------------------------
    #{exchangerName}
    |]

htmlContent :: Text -> Text -> Text -> Text -> TL.Text
htmlContent _email url exchangerName exchangerHost = renderHtml [shamlet|
    <html>
        <head>
        <body>
            <h3>#{exchangerName} | Регистрация
            <h4>Необходимо подтверждение электронной почты
            <br>
            <br>
            <p>
                Уважаемый пользователь,
            <p>
                для завершения регистрации на сайте #{exchangerHost} пройдите по
                <a href="#{url}">этой ссылке
            <p>
            <p>Если это письмо пришло Вам по ошибке, просто проигнорируйте его.
            <p>
                <small>
                    Прямая ссылка: #{url}

            <p style="text-align: right;">
                <i>
                    С уважением, Администрация #{exchangerHost}.
    |]


maybeReferrer :: Handler (Maybe (Entity Referrer))
maybeReferrer = do
    token <- referrerCookie
    case token of
        Nothing -> return Nothing
        Just t -> runDB $
            getBy (UniqueReferrerToken t)

addReferral ::
    MonadIO m => Entity Referrer -> UserId -> SqlPersistT m (Entity Referral)
addReferral (Entity ref _) user = insertEntity (Referral user ref)

cleanUpRefCookie :: Handler ()
cleanUpRefCookie = do
    name <- appRefTokenCookieName . appSettings <$> getYesod
    let deletedRef = name <> "=deleted"
    addHeader "Set-Cookie" $
        deletedRef <> "; path=/; expires=Thu, 01 Jan 1970 00:00:00 GMT"

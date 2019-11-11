{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.SignUp where

import           Form.Auth.SignUp
import           Import
import           Local.Persist.UserRole
import           Settings.MailRu               ( password, serverName, smtpPort,
                                                 username )
import           Type.Auth.SignUp              ( SignUpFormData (..) )

import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           Network.HaskellNet.SMTP
import           Network.HaskellNet.SMTP.SSL
import           Text.Blaze.Html.Renderer.Text ( renderHtml )
import           Yesod.Auth.Util.PasswordStore ( makePassword )


authType = PLAIN
from = "noreply@outb.info"
subject = "Подтвердите ваш электронный ящик"
plainBody = textContent
htmlBody = htmlContent


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
                        sendEmailActivationMessage email key
                        defaultLayout $(widgetFile "auth/verify-message")
            else defaultLayout $(widgetFile "auth/verify-message")
        _ -> do
            let mayError = Nothing :: Maybe Text
            defaultLayout $(widgetFile "auth/signup")
  where

    createUniqueLogin :: Text -> Text -> Text -> Handler UserCreateResult
    createUniqueLogin login pass key = runDB $ do
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
                return CreateSuccess

    sendEmailActivationMessage :: Text -> Text -> Handler ()
    sendEmailActivationMessage email key = do
        urlRender <- getUrlRender
        let verUrl = urlRender $ SignUpVerifyR email key
        liftIO $ do
            conn <- connectSMTPSSLWithSettings
                (unpack serverName)
                (defaultSettingsSMTPSSL { sslPort = smtpPort })
            authSuccess <-
                Network.HaskellNet.SMTP.SSL.authenticate
                    PLAIN
                    (unpack username)
                    (unpack password)
                    conn
            if authSuccess
                then sendMimeMail (T.unpack email)
                                  from
                                  subject
                                  (plainBody email verUrl)
                                  (htmlBody email verUrl)
                                  []
                                  conn
                else putStrLn "Authentication failed."
            closeSMTP conn

data UserCreateResult
    = CreateError Text
    | CreateSuccess

textContent :: Text -> Text -> TL.Text
textContent email url = renderHtml [shamlet|
    Необходимо подтверждение электронной почты.

    Для завершения регистрации на сайте #{exchangerHost} пройдите по ссылке:
    #{url}

    Если это письмо пришло Вам по ошибке, просто проигнорируйте его.

    ------------------------------------------
    #{exchangerName}
    |]

htmlContent :: Text -> Text -> TL.Text
htmlContent email url = renderHtml [shamlet|
    <html>
        <head>
        <body>
            <h3>#{exchangerName}
            <h4>Необходимо подтверждение электронной почты
            <br>
            <br>
            <p>Для завершения регистрации на сайте #{exchangerHost} пройдите по ссылке:
            <p>#{url}
            <p>
            <p>Если это письмо пришло Вам по ошибке, просто проигнорируйте его.
    |]

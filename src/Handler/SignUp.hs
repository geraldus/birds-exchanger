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
import           Type.Auth.SignUp              ( SignUpFormData (..) )
import           Utils.App.Common              ( sendNoReplyEmail )
import           Utils.Common                  ( projectNameHost )

import qualified Data.Text.Lazy                as TL
import           Text.Blaze.Html.Renderer.Text ( renderHtml )
import           Yesod.Auth.Util.PasswordStore ( makePassword )


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
        FormSuccess (SignUpFormData email' pass conf) -> if pass == conf
            then do
                key <- appNonce128urlT
                let email = toLower email'
                createResult <- createUniqueLogin email pass key
                case createResult of
                    CreateError e -> do
                        let mayError = Just e
                        defaultLayout $(widgetFile "auth/signup")
                    CreateSuccess -> do
                        cleanUpRefCookie
                        sendEmailActivationMessage email key
                        defaultLayout $(widgetFile "auth/verify-message")
            else do
                let email = toLower email'
                defaultLayout $(widgetFile "auth/verify-message")
        _ -> do
            let mayError = Nothing :: Maybe Text
            defaultLayout $(widgetFile "auth/signup")
  where
    createUniqueLogin :: Text -> Text -> Text -> Handler UserCreateResult
    createUniqueLogin login' pass key = do
        let login = toLower login'
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
        urlRender     <- getUrlRender
        messageRender <- (getMessageRender :: Handler (AppMessage -> Text))
        projType <- appType . appSettings <$> getYesod
        let verUrl = urlRender $ SignUpVerifyR email key
        let (exName, exHost) = projectNameHost projType
        let txt     = textContent email verUrl exName exHost
            html    = htmlContent email verUrl exName exHost
            subject = messageRender MsgEmailSubjectConfirmEmail
        void . liftIO $ sendNoReplyEmail projType messageRender email subject txt html

data UserCreateResult
    = CreateError Text
    | CreateSuccess

-- ^ TODO move code to template
textContent :: Text -> Text -> Text -> Text -> TL.Text
textContent _email url exchangerName exchangerHost = renderHtml [shamlet|
    Необходимо подтверждение электронной почты.

    Для завершения регистрации на сайте #{exchangerHost} пройдите по ссылке:
    #{url}

    Если это письмо пришло Вам по ошибке, просто проигнорируйте его.

    ----------------------------------------------------------------------------
    #{exchangerName}
    |]

-- ^ TODO move code to template
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

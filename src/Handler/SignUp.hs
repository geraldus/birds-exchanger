{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.SignUp where

import           Form.Auth.SignUp
import           Import
import           Local.Persist.UserRole
import           Type.Auth.SignUp              (SignUpFormData (..))

import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           Network.HaskellNet.SMTP
import           Network.HaskellNet.SMTP.SSL
import           Text.Blaze.Html.Renderer.Text (renderHtml)



-- | Your settings
server = "smtp.mail.ru"
smtpPort = toEnum 465
username = "mailer@prizmone.bizml.ru"
password = "|DfDlVwB7zg0"
authType = PLAIN
from = "mailer@prizmone.bizml.ru"
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
        setTitle $ toHtml ("Регистрация / Вход" :: Text)
        $(widgetFile "auth/signup")


postSignUpR :: Handler Html
postSignUpR = do
    ((signUpDataResult, widget), enctype) <- runFormPost signUpForm
    case signUpDataResult of
        FormSuccess (SignUpFormData email pass conf) -> if (pass == conf)
            then do
                key          <- appNonce128urlT
                createResult <- runDB $ do
                    mayExistingUser <- getBy $ UniqueEmail email
                    case mayExistingUser of
                        Just _ ->
                            return
                                $ CreateError
                                      "Пользователь с таким эл.адресом уже существует"
                        Nothing -> do
                            let newUser = User email (Just pass) Client
                            userId <- insert newUser
                            let newEmail = Email email (Just userId) (Just key)
                            emailId <- insert newEmail
                            return CreateSuccess
                case createResult of
                    CreateError e -> do
                        let mayError = Just e
                        defaultLayout $(widgetFile "auth/signup")
                    CreateSuccess -> do
                        urlRender <- getUrlRender
                        let verUrl = urlRender $ SignUpVerifyR email key
                        liftIO $ do
                            conn <- connectSMTPSSLWithSettings
                                server
                                (defaultSettingsSMTPSSL { sslPort = smtpPort })
                            authSuccess <-
                                Network.HaskellNet.SMTP.SSL.authenticate
                                    PLAIN
                                    username
                                    password
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
                        defaultLayout $ $(widgetFile "auth/verify-message")
            else defaultLayout $(widgetFile "auth/verify-message")
        _ -> do
            let mayError = Nothing :: Maybe Text
            defaultLayout $(widgetFile "auth/signup")


data UserCreateResult
    = CreateError Text
    | CreateSuccess

textContent :: Text -> Text -> TL.Text
textContent email url = renderHtml $ [shamlet|
    Необходимо подтверждение электронной почты

    Для завершения регистрации в обменнике (#{exchangerHost}) пройдите по ссылке:
    #{url}

    Если это письмо пришло Вам по ошибке, просто проигнорируйте его.
    |]

htmlContent :: Text -> Text -> TL.Text
htmlContent email url = renderHtml $ [shamlet|
    <html>
        <head>
        <body>
            <h3>#{exchangerName}
            <h4>Необходимо подтверждение электронной почты
            <p>
            <p>Для завершения регистрации в обменнике (#{exchangerHost}) пройдите по ссылке:
            <p>#{url}
            <p>
            <p>Если это письмо пришло Вам по ошибке, просто проигнорируйте его.
    |]

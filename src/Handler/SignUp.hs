{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.SignUp where

import           Import
import           Type.Auth.SignUp               ( SignUpFormData(..) )
import           Form.Auth.SignUp

import qualified Crypto.Nonce                  as CN
import           Network.HaskellNet.SMTP
import qualified Data.Text                     as T
import           Network.HaskellNet.SMTP.SSL


-- | Your settings
server = "smtp.mail.ru"
smtpPort = toEnum 465
username = "mailer@prizmone.bizml.ru"
password = "|DfDlVwB7zg0"
authType = PLAIN
from = "mailer@prizmone.bizml.ru"
subject = "Подтвердите ваш электронный ящик"
plainBody = fromStrict textContent
htmlBody = fromStrict htmlContent


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
                -- TODO: FIXME: Move nonce generator to AppSettings
                nonceGen     <- CN.new
                key          <- CN.nonce128urlT nonceGen
                createResult <- runDB $ do
                    mayExistingUser <- getBy $ UniqueEmail email
                    case mayExistingUser of
                        Just _ ->
                            return
                                $ CreateError
                                      "Пользователь с таким эл.адресом уже существует"
                        Nothing -> do
                            let newUser = User email (Just pass)
                            userId <- insert newUser
                            let newEmail = Email email (Just userId) (Just key)
                            emailId <- insert newEmail
                            return CreateSuccess
                case createResult of
                    CreateError e -> do
                        let mayError = Just e
                        defaultLayout $ $(widgetFile "auth/signup")
                    CreateSuccess -> do
                        liftIO $ do
                            conn <- connectSMTPSSLWithSettings
                                server
                                (defaultSettingsSMTPSSL { sslPort = smtpPort })
                            print "Connected"
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
                                                  plainBody
                                                  htmlBody
                                                  []
                                                  conn
                                else putStrLn "Authentication failed."
                            closeSMTP conn
                        defaultLayout $ $(widgetFile "auth/verify-message")
            else defaultLayout $ $(widgetFile "auth/verify-message")
        _ -> do
            let mayError = Nothing :: Maybe Text
            defaultLayout $(widgetFile "auth/signup")


data UserCreateResult
    = CreateError Text
    | CreateSuccess

textContent :: Text
textContent = "Привет!  Это письмо отправлено из обменника PRIZM через MAILGUN"

htmlContent :: Text
htmlContent =
    "<html><head></head><body><h3>Привет!</h3><p>Это письмо отправлено из обменника <b>PRIZM</b> через <i>MAILGUN</i></p></body></html>"

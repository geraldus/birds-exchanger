{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.SignUp where

import Import
import Type.Auth.SignUp (SignUpFormData(..))
import Form.Auth.SignUp

import qualified Crypto.Nonce as CN


getSignUpR :: Handler Html
getSignUpR = do
    mUser <- maybeAuthId
    when (isJust mUser) $
        redirect HomeR
    (widget, enctype) <- generateFormPost signUpForm
    defaultLayout $ do
        let mayError = Nothing :: Maybe Text
        setTitle $ toHtml ("Регистрация / Вход" :: Text)
        $(widgetFile "auth/signup")

postSignUpR :: Handler Html
postSignUpR = do
    ((signUpDataResult, widget), enctype) <- runFormPost signUpForm
    case signUpDataResult of
        FormSuccess (SignUpFormData email pass conf) ->
            if (pass == conf)
                then do
                    -- TODO: FIXME: Move nonce generator to AppSettings
                    nonceGen <- CN.new
                    key <- CN.nonce128urlT nonceGen
                    createResult <- runDB $ do
                        mayExistingUser <- getBy $ UniqueEmail email
                        case mayExistingUser of
                            Just _ -> return $ CreateError "Пользователь с таким эл.адресом уже существует"
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
                        CreateSuccess -> 
                            defaultLayout $ $(widgetFile "auth/verify-message")
                else defaultLayout $ $(widgetFile "auth/verify-message")


data UserCreateResult
    = CreateError Text
    | CreateSuccess



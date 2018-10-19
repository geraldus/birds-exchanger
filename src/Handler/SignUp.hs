{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.SignUp where

import Import


getSignUpR :: Handler Html
getSignUpR = do
    (widget, enctype) <- generateFormPost signUpForm
    defaultLayout $ do
        setTitle $ toHtml ("Регистрация / Вход" :: Text)
        $(widgetFile "auth/signup")


data SignUpFormData = SignUpFormData
    { signUpFormDataEmail :: Text
    , signUpFormDataPassword :: Text
    , signUpFormDataPasswordConfirm :: Text
    }


data SignUpData = SignUpData
    { signUpDataEmail :: Text
    , signUpDataPassword :: Text
    }


signUpForm :: Html -> MForm Handler (FormResult SignUpFormData, Widget)
signUpForm = renderDivs $ SignUpFormData
    <$> areq textField "Email" Nothing
    <*> areq textField "Password" Nothing
    <*> areq textField "Confirm Password" Nothing
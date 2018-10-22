{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.SignUp where

import Import
import Type.Auth.SignUp (SignUpFormData(..))
import Form.Auth.SignUp



getSignUpR :: Handler Html
getSignUpR = do
    (widget, enctype) <- generateFormPost signUpForm
    defaultLayout $ do
        setTitle $ toHtml ("Регистрация / Вход" :: Text)
        $(widgetFile "auth/signup")





    <*> areq textField "Confirm Password" Nothing
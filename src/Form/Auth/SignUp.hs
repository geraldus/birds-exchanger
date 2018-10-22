{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Form.Auth.SignUp where

import Import
import Type.Auth.SignUp


signUpForm :: Html -> MForm Handler (FormResult SignUpFormData, Widget)
signUpForm = renderDivs $ SignUpFormData
    <$> areq textField "Email" Nothing
    <*> areq passwordField "Password" Nothing
    <*> areq passwordField "Confirm Password" Nothing
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Form.Auth.SignUp where

import Import
import Type.Auth.SignUp


signUpForm :: Html -> MForm Handler (FormResult SignUpFormData, Widget)
signUpForm = renderDivs $ SignUpFormData
    <$> areq textField "Эл.почта" Nothing
    <*> areq passwordField "Пароль" Nothing
    <*> areq passwordField "Подтверждение пароля" Nothing
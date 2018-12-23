{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Form.Auth.SignUp where

import Import
import Type.Auth.SignUp


signUpForm :: Html -> MForm Handler (FormResult SignUpFormData, Widget)
signUpForm extra = do
    eid <- newIdent
    pid <- newIdent
    cid <- newIdent
    (email, emailV) <- mreq emailField (fsAddPlaceholder (fsBs4WithId eid) "Ваш эл.ящик") Nothing
    (passw, passwV) <- mreq passwordField (fsBs4WithId pid) Nothing
    (confi, confiV) <- mreq passwordField (fsBs4WithId cid) Nothing
    let widget = [whamlet|
            #{extra}
            <div .form-group.row>
                <label for=#{eid}>Эл.почта
                ^{fvInput emailV}
            <div .form-group.row>
                <label for=#{pid}>Пароль
                ^{fvInput passwV}
            <div .form-group.row>
                <label for=#{cid}>Подтверждение пароля
                ^{fvInput confiV}
            |]
    let res = SignUpFormData
            <$> email
            <*> passw
            <*> confi
    return (res, widget)

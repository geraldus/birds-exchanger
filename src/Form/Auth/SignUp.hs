{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Form.Auth.SignUp where

import Import
import Type.Auth.SignUp


signUpForm :: Html -> MForm Handler (FormResult SignUpFormData, Widget)
signUpForm extra = do
    eid <- newIdent
    pid <- newIdent
    cid <- newIdent
    (email', emailV) <- mreq emailField (fsAddPlaceholder (fsBs4WithId eid) "Ваш эл.ящик") Nothing
    (passw, passwV)  <- mreq passwordField (fsBs4WithId pid) Nothing
    (confi, confiV)  <- mreq passwordField (fsBs4WithId cid) Nothing
    let email = toLower <$> email'
    (_, termsV) <- mreq
            checkBoxField
            (fsWithClasses
                [ "form-check-input" ]
                ""
                Nothing
                (Just "terms-check")
                Nothing
                [("required", "required")])
            Nothing
    let widget = [whamlet|
            #{extra}
            <div .form-group>
                <label for=#{eid}>_{MsgEmailAddress}
                ^{fvInput emailV}
            <div .form-group>
                <label for=#{pid}>_{MsgPassword}
                ^{fvInput passwV}
            <div .form-group>
                <label for=#{cid}>_{MsgPasswordConfirmation}
                ^{fvInput confiV}
            <div .form-check>
                ^{fvInput termsV}
                <label .form-check-label for="terms-check">
                    <small>_{MsgIAcceptTermsOfUseText}
            |]
    let res = SignUpFormData
            <$> email
            <*> passw
            <*> confi
    return (res, widget)

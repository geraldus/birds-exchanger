{-# LANGUAGE NoImplicitPrelude #-}
module Type.Auth.SignUp where

    
import ClassyPrelude.Yesod


data SignUpFormData = SignUpFormData
    { signUpFormDataEmail :: Text
    , signUpFormDataPassword :: Text
    , signUpFormDataPasswordConfirm :: Text
    }


data SignUpData = SignUpData
    { signUpDataEmail :: Text
    , signUpDataPassword :: Text
    }

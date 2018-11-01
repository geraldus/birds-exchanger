{-# LANGUAGE NoImplicitPrelude #-}
module Type.Auth.SuperUser where

import ClassyPrelude.Yesod


data SuperUser = SuperUser
    { suName :: Text
    , suPassword :: Text }
    deriving Show
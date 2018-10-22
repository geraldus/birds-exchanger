{-# LANGUAGE OverloadedStrings #-}
module Local.Auth
    ( module Type
    , superUsers ) where

import Type.Auth.SuperUser as Type


superUsers :: [SuperUser]
superUsers = [SuperUser "gman" "123123"]

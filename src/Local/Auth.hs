{-# LANGUAGE OverloadedStrings #-}
module Local.Auth
    ( module Type
    , superUsers
    , module Plugin
    ) where

import Type.Auth.SuperUser as Type
import Local.Auth.Plugin   as Plugin


superUsers :: [ SuperUser ]
superUsers =
    [ SuperUser "gman" "alpha3003omega"
    , SuperUser "ukropa" "Vanya999Velikiy!" ]

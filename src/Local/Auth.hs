{-# LANGUAGE OverloadedStrings #-}
module Local.Auth
    ( module Type
    , superUsers
    , module Plugin
    ) where

import           Local.Auth.Plugin   as Plugin
import           Type.Auth.SuperUser as Type


superUsers :: [ SuperUser ]
superUsers =
    [ SuperUser "gman" "alpha3003omega"
    , SuperUser "ukropa" "Vanya999Velikiy!"
    , SuperUser "xunder" "GaoHo~555!StrongPower"
    , SuperUser "operator1" "~operator1_SDFnh!"
    , SuperUser "operator2" "~operator2_kj#Scx"
    , SuperUser "operator3" "~operator3_GFm%lj"
    , SuperUser "operator4" "~operator4_Vn;kl2"
    ]

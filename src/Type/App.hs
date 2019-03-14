{-# LANGUAGE GADTs #-}
module Type.App where

import           Import.NoFoundation


data MenuItem m where
    MenuItem :: RenderRoute m => {
          menuItemLabel          :: Text
        , menuItemRoute          :: Route m
        , menuItemAccessCallback :: Bool
        } -> MenuItem m

data MenuTypes m
    = NavbarLeft (MenuItem m)
    | NavbarRight (MenuItem m)

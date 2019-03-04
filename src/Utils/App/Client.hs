{-# LANGUAGE QuasiQuotes #-}
module Utils.App.Client where

import           Import

import           Utils.Common


dateTimeRowW :: UTCTime -> Widget
dateTimeRowW t =
    toWidget [whamlet|
        <small>
            ^{dateTimeRowWM t}|]

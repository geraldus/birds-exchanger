module Utils.App.Client where

import           Import

import           Utils.Common


requestTimeW :: UTCTime -> Widget
requestTimeW t =
    toWidget [whamlet|
        <small>
            ^{dateTimeRowW t}|]

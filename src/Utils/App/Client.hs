module Utils.App.Client where

import           Import

import           Utils.App.Common


requestTimeW :: UTCTime -> Widget
requestTimeW t =
    toWidget [whamlet|
        <small .text-muted>
            ^{dateTimeRowW t}|]

{-# LANGUAGE TemplateHaskell #-}
module Local.Persist.ExchangeOrder where

import Prelude

import Database.Persist.TH
import Data.Time.Clock (UTCTime)


data ExchageOrderStatus
    = Created UTCTime
    | Cancelled UTCTime
    | PartiallyExecuted UTCTime Int  -- ^ last mod time and executed cents total
    | Executed UTCTime
    deriving (Show, Read, Eq)
derivePersistField "ExchageOrderStatus"
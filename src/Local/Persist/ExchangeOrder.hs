{-# LANGUAGE TemplateHaskell #-}
module Local.Persist.ExchangeOrder where

import           ClassyPrelude.Yesod

import           Data.Time.Clock                ( UTCTime )


data ExchangeOrderStatus
    = Created UTCTime
    | Cancelled UTCTime
    | PartiallyExecuted UTCTime Int  -- ^ last mod time and executed cents total
    | Executed UTCTime
    deriving (Show, Read, Eq)
derivePersistField "ExchangeOrderStatus"

data ProfitType
    = DepositFee
    | WithdrawalFee
    | ExchangeFee
    | ExchangeDiff
    deriving (Show, Read, Eq)
derivePersistField "ProfitType"


data ExchangePair
    = ExchangePzmRur
    | ExchangeRurPzm
    deriving (Show, Read, Eq)
derivePersistField "ExchangePair"

instance PathPiece ExchangePair where
    toPathPiece = pack . show
    fromPathPiece = readMay
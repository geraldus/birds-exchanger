{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Local.Persist.Exchange where

import           ClassyPrelude.Yesod

import           Data.Aeson
import           Data.Time.Clock     ( UTCTime )


data ExchangeOrderStatus
    = Created UTCTime
    | Cancelled UTCTime
    | PartiallyExecuted UTCTime Int  -- ^ last mod time and executed cents total
    | Executed UTCTime
    deriving (Show, Read, Eq, Generic)
instance FromJSON ExchangeOrderStatus
instance ToJSON ExchangeOrderStatus
instance ToJSONKey ExchangeOrderStatus
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
    | ExchangePzmOur
    | ExchangeOurPzm
    | ExchangeRurOur
    | ExchangeOurRur
    deriving (Show, Read, Eq, Generic)
instance ToJSON ExchangePair where
    toJSON = String . pack . show
instance FromJSON ExchangePair where
    parseJSON = withText "ExchangePair Haskell Type Value" $ \v -> return $
        case readMay v of
            Just p  -> p
            Nothing -> error "Haskell Value read error"
instance ToJSONKey ExchangePair
instance Hashable ExchangePair where
    hash = hash . show
derivePersistField "ExchangePair"

instance PathPiece ExchangePair where
    toPathPiece = pack . show
    fromPathPiece = readMay

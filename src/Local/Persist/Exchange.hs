{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Local.Persist.Exchange where

import           Local.Persist.Currency ( CryptoCurrency (..), Currency (..),
                                          FiatCurrency (..) )

import           ClassyPrelude.Yesod
import           Data.Aeson
import           Data.Time.Clock        ( UTCTime )


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
    deriving (Show, Read, Eq, Generic)

instance ToJSON ProfitType

instance FromJSON ProfitType

derivePersistField "ProfitType"


-- | = Exchange Pair
--   == Also used as exchange direction representation
--   //TODO://
--   Abstract this data type over @currency code@ which could be an
--   'Int' meaning international currency code value or 'String'
--   meaning international or commonly used currency string code, e.g.
--   USD, RUB, PZM, OURO
--   //TODO://
--   > Split 'ExchangePair' to Exchange Direction and Exchange Pair
--   > Former holds string CODE from and string code to.
--   > Latter holds direction from -> to and backward from <- to
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

exchangePairUnsafe :: Currency -> Currency -> ExchangePair
-- | RUB
exchangePairUnsafe (FiatC RUB)   (CryptoC PZM) = ExchangeRurPzm
exchangePairUnsafe (FiatC RUB)   (CryptoC OURO) = ExchangeRurOur
-- | PZM
exchangePairUnsafe (CryptoC PZM) (FiatC RUB)   = ExchangePzmRur
exchangePairUnsafe (CryptoC PZM) (CryptoC OURO) = ExchangePzmOur
-- | OURO
exchangePairUnsafe (CryptoC OURO) (FiatC RUB)   = ExchangeOurRur
exchangePairUnsafe (CryptoC OURO) (CryptoC PZM) = ExchangeOurPzm
-- | Other
exchangePairUnsafe c1 c2 = error $
    "Unexpected exchange pair: " <> show c1 <> ", "  <> show c2


exchangePair :: Currency -> Currency -> Maybe ExchangePair
-- RUB
exchangePair (FiatC RUB)   (CryptoC PZM) = Just ExchangeRurPzm
exchangePair (FiatC RUB)   (CryptoC OURO) = Just ExchangeRurOur
-- PZM
exchangePair (CryptoC PZM) (FiatC RUB)   = Just ExchangePzmRur
exchangePair (CryptoC PZM) (CryptoC OURO) = Just ExchangePzmOur
-- OURO
exchangePair (CryptoC OURO) (FiatC RUB)   = Just ExchangeOurRur
exchangePair (CryptoC OURO) (CryptoC PZM) = Just ExchangeOurPzm
-- Other
exchangePair _ _ = Nothing

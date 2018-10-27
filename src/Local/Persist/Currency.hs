{-# LANGUAGE TemplateHaskell #-}
module Local.Persist.Currency where


import Database.Persist.TH
import Prelude


data CurrencyType
    = FiatT
    | CryptoT
    deriving (Show, Read, Eq)
derivePersistField "CurrencyType"

data FiatCurrency
    = RUR
    | USD
    deriving (Show, Read, Eq)
derivePersistField "FiatCurrency"

data CryptoCurrency
    = PZM
    | ETH
    | BTC
    deriving (Show, Read, Eq)
derivePersistField "CryptoCurrency"


data Currency
    = FiatC FiatCurrency
    | CryptoC CryptoCurrency
    deriving (Show, Read, Eq)
derivePersistField "Currency"


data FiatPaymentMethod
    = Card2CardFPM
    | QiwiFPM
    deriving (Show, Read, Eq)
derivePersistField "FiatPaymentMethod"

data PaymentMethod
    = FiatPM FiatPaymentMethod FiatCurrency
    | CryptoPM CryptoCurrency
    deriving (Show, Read, Eq)
derivePersistField "PaymentMethod"
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Local.Persist.Currency where

import           ClassyPrelude.Yesod

import           Data.Aeson


data CurrencyType
    = FiatT
    | CryptoT
    deriving (Generic, Show, Read, Eq)
derivePersistField "CurrencyType"

data FiatCurrency
    = RUB
    | USD
    deriving (Generic, Show, Read, Eq)
derivePersistField "FiatCurrency"

data CryptoCurrency
    = PZM
    | ETH
    | BTC
    | OURO
    deriving (Generic, Show, Read, Eq)
derivePersistField "CryptoCurrency"

data Currency
    = FiatC FiatCurrency
    | CryptoC CryptoCurrency
    deriving (Generic, Show, Read, Eq)
derivePersistField "Currency"

instance ToJSON CurrencyType where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON CurrencyType

instance ToJSON FiatCurrency where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON FiatCurrency

instance ToJSON CryptoCurrency where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON CryptoCurrency

instance ToJSON Currency where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Currency


pzmC :: Currency
pzmC = CryptoC PZM

{-# DEPRECATED rurC "Since 0.6.13.  Please use `rubC` instead." #-}
rurC :: Currency
rurC = rubC

rubC :: Currency
rubC = FiatC RUB

{-# DEPRECATED ourC "Since 0.6.9.  Please use `ouroC` instead." #-}
ourC :: Currency
ourC = ouroC

ouroC :: Currency
ouroC = CryptoC OURO


fCurrencyCodeT :: FiatCurrency -> Text
fCurrencyCodeT USD = "USD"
fCurrencyCodeT RUB = "RUB"

cCurrencyCodeT :: CryptoCurrency -> Text
cCurrencyCodeT PZM = "PZM"
cCurrencyCodeT BTC = "BTC"
cCurrencyCodeT ETH = "ETH"
cCurrencyCodeT OURO = "OURO"

fiatCurrencyCodeT :: FiatCurrency -> Text
fiatCurrencyCodeT = toLower . fCurrencyCodeT

cryptoCurrencyCodeT :: CryptoCurrency -> Text
cryptoCurrencyCodeT OURO = "ouro"
cryptoCurrencyCodeT x   = toLower $ cCurrencyCodeT x

currencyCodeT' :: Currency -> Text
currencyCodeT' (FiatC c) = fiatCurrencyCodeT c
currencyCodeT' (CryptoC c) = cryptoCurrencyCodeT c

fCurrencyTShort :: FiatCurrency -> Text
fCurrencyTShort USD = "$"
fCurrencyTShort RUB = "₽"

fCurrencyTLong :: FiatCurrency -> Text
fCurrencyTLong USD = "доллар США"
fCurrencyTLong RUB = "российский рубль"

currencyNameT :: Currency -> Text
currencyNameT (FiatC fc) = fCurrencyTLong fc
currencyNameT (CryptoC cc) = cCurrencyTLong cc

cCurrencyTShort :: CryptoCurrency -> Text
cCurrencyTShort = cCurrencyCodeT

cCurrencyTLong :: CryptoCurrency -> Text
cCurrencyTLong PZM = "Prizm"
cCurrencyTLong BTC = "Bitcoin"
cCurrencyTLong ETH = "Etherium"
cCurrencyTLong OURO = "Ouroboros"

currencySymbol :: Currency -> Text
currencySymbol (FiatC USD) = "$"
currencySymbol (FiatC RUB) = "₽"
currencySymbol c = (toUpper . currencyCodeT') c

currencyCodeT :: Currency -> Text
currencyCodeT (FiatC c)   = fCurrencyCodeT c
currencyCodeT (CryptoC c) = cCurrencyCodeT c
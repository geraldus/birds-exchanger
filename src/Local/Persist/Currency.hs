{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Local.Persist.Currency where

import           ClassyPrelude.Yesod
import           Data.Aeson
import           Text.Read           ( readMaybe )


data CurrencyType
    = FiatT
    | CryptoT
    deriving (Generic, Show, Read, Eq)
derivePersistField "CurrencyType"

data FiatCurrency
    = RUR
    | USD
    deriving (Generic, Show, Read, Eq)
derivePersistField "FiatCurrency"

data CryptoCurrency
    = PZM
    | ETH
    | BTC
    | OUR
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
rubC = FiatC RUR

{-# DEPRECATED ourC "Since 0.6.9.  Please use `ouroC` instead." #-}
ourC :: Currency
ourC = ouroC

ouroC :: Currency
ouroC = CryptoC OUR


fCurrencyCodeT :: FiatCurrency -> Text
fCurrencyCodeT USD = "USD"
fCurrencyCodeT RUR = "RUB"

cCurrencyCodeT :: CryptoCurrency -> Text
cCurrencyCodeT PZM = "PZM"
cCurrencyCodeT BTC = "BTC"
cCurrencyCodeT ETH = "ETH"
cCurrencyCodeT OUR = "OUR"

fiatCurrencyCodeT :: FiatCurrency -> Text
fiatCurrencyCodeT = toLower . fCurrencyCodeT

cryptoCurrencyCodeT :: CryptoCurrency -> Text
cryptoCurrencyCodeT OUR = "ouro"
cryptoCurrencyCodeT x   = toLower $ cCurrencyCodeT x

currencyCodeT' :: Currency -> Text
currencyCodeT' (FiatC c) = fiatCurrencyCodeT c
currencyCodeT' (CryptoC c) = cryptoCurrencyCodeT c

fCurrencyTShort :: FiatCurrency -> Text
fCurrencyTShort USD = "$"
fCurrencyTShort RUR = "₽"

fCurrencyTLong :: FiatCurrency -> Text
fCurrencyTLong USD = "доллар США"
fCurrencyTLong RUR = "российский рубль"

currencyNameT :: Currency -> Text
currencyNameT (FiatC fc) = fCurrencyTLong fc
currencyNameT (CryptoC cc) = cCurrencyTLong cc

cCurrencyTShort :: CryptoCurrency -> Text
cCurrencyTShort = cCurrencyCodeT

cCurrencyTLong :: CryptoCurrency -> Text
cCurrencyTLong PZM = "Prizm"
cCurrencyTLong BTC = "Bitcoin"
cCurrencyTLong ETH = "Etherium"
cCurrencyTLong OUR = "Ouroboros"

{-# DEPRECATED currSign "Since 0.6.13.  Please use `currencySymbol` to get correct crypto currency shortand codes." #-}
currSign :: Currency -> Text
currSign (CryptoC c) = cCurrencyCodeT c
currSign c = currencySymbol c

currencySymbol :: Currency -> Text
currencySymbol (FiatC USD) = "$"
currencySymbol (FiatC RUR) = "₽"
currencySymbol c = (toUpper . currencyCodeT') c

currencyCodeT :: Currency -> Text
currencyCodeT (FiatC c)   = fCurrencyCodeT c
currencyCodeT (CryptoC c) = cCurrencyCodeT c


data FiatTransferMethod
    = SberBankCard2CardFTM
    | AlphaBankCard2CardFTM
    | TinkoffBankCard2CardFTM
    | PayPalTransferFTM
    | QiwiFTM
    deriving (Generic, Show, Read, Eq)
derivePersistField "FiatTransferMethod"

data TransferMethod
    = FiatTM FiatTransferMethod FiatCurrency
    | CryptoTM CryptoCurrency
    deriving (Generic, Show, Read, Eq)
derivePersistField "TransferMethod"

instance ToJSON FiatTransferMethod where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON FiatTransferMethod

instance ToJSON TransferMethod where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON TransferMethod

instance PathPiece TransferMethod where
    fromPathPiece = readMaybe . unpack
    toPathPiece = pack . show


tmSber :: FiatCurrency -> TransferMethod
tmSber = FiatTM SberBankCard2CardFTM

tmAlpha :: FiatCurrency -> TransferMethod
tmAlpha = FiatTM AlphaBankCard2CardFTM

tmTinkoff :: FiatCurrency -> TransferMethod
tmTinkoff = FiatTM TinkoffBankCard2CardFTM

tmQiwi :: FiatCurrency -> TransferMethod
tmQiwi = FiatTM QiwiFTM

tmPayPal :: FiatCurrency -> TransferMethod
tmPayPal = FiatTM PayPalTransferFTM

ftmSberRur :: TransferMethod
ftmSberRur = tmSber RUR

ftmAlphaRur :: TransferMethod
ftmAlphaRur = tmAlpha RUR

ftmTinkoffRur :: TransferMethod
ftmTinkoffRur = tmTinkoff RUR

ftmQiwiRur :: TransferMethod
ftmQiwiRur = tmQiwi RUR

ftmPayPalRur :: TransferMethod
ftmPayPalRur = tmPayPal RUR

ftmPayPalUsd :: TransferMethod
ftmPayPalUsd = tmPayPal USD

ctmPzm :: TransferMethod
ctmPzm = CryptoTM PZM

ctmBtc :: TransferMethod
ctmBtc = CryptoTM BTC

ctmEth :: TransferMethod
ctmEth = CryptoTM ETH

ctmOur :: TransferMethod
ctmOur = CryptoTM OUR


fTmTShort :: FiatTransferMethod -> Html
fTmTShort SberBankCard2CardFTM    = "СберБанк"
fTmTShort AlphaBankCard2CardFTM   = "АльфаБанк"
fTmTShort TinkoffBankCard2CardFTM = "Тинькофф Банк"
fTmTShort PayPalTransferFTM       = "PayPal"
fTmTShort QiwiFTM                 = "Qiwi"

tmTShort :: TransferMethod -> Html
tmTShort (FiatTM m fc) = [shamlet|
    #{fTmTShort m} #
    <small .text-muted>
        #{fCurrencyCodeT fc} / #{fCurrencyTShort fc}
    |]
tmTShort (CryptoTM cc) = [shamlet|
    #{cCurrencyTLong cc} #
        <small .text-muted>#{cCurrencyCodeT cc}
    |]

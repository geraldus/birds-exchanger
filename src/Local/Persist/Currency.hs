{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Local.Persist.Currency where

import           Database.Persist.TH
import           ClassyPrelude.Yesod
import           Text.Read                      ( readMaybe )


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

pzmC :: Currency
pzmC = CryptoC PZM

rurC :: Currency
rurC = FiatC RUR

fCurrencyCodeT :: FiatCurrency -> Text
fCurrencyCodeT USD = "USD"
fCurrencyCodeT RUR = "RUR"

cCurrencyCodeT :: CryptoCurrency -> Text
cCurrencyCodeT PZM = "PZM"
cCurrencyCodeT BTC = "BTC"
cCurrencyCodeT ETH = "ETH"

fCurrencyTShort :: FiatCurrency -> Text
fCurrencyTShort USD = "$"
fCurrencyTShort RUR = "₽"

fCurrencyTLong :: FiatCurrency -> Text
fCurrencyTLong USD = "доллар США"
fCurrencyTLong RUR = "российский рубль"

cCurrencyTShort :: CryptoCurrency -> Text
cCurrencyTShort = cCurrencyCodeT

cCurrencyTLong :: CryptoCurrency -> Text
cCurrencyTLong PZM = "Prizm"
cCurrencyTLong BTC = "Bitcoin"
cCurrencyTLong ETH = "Etherium"


currSign :: Currency -> Text
currSign (FiatC   USD) = "$"
currSign (FiatC   RUR) = "₽"
currSign (CryptoC c  ) = cCurrencyCodeT c


data FiatTransferMethod
    = SberBankCard2CardFTM
    | AlphaBankCard2CardFTM
    | TinkoffBankCard2CardFTM
    | PayPalTransferFTM
    | QiwiFTM
    deriving (Show, Read, Eq)
derivePersistField "FiatTransferMethod"

data TransferMethod
    = FiatTM FiatTransferMethod FiatCurrency
    | CryptoTM CryptoCurrency
    deriving (Show, Read, Eq)
derivePersistField "TransferMethod"

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


instance PathPiece TransferMethod where
    fromPathPiece = readMaybe . unpack
    toPathPiece = pack . show

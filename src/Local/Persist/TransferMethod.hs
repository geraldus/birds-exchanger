{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Local.Persist.TransferMethod where

import           ClassyPrelude.Yesod

import           Local.Persist.Currency

import           Data.Aeson
import           Text.Read              ( readMaybe )


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

ftmSberRub :: TransferMethod
ftmSberRub = tmSber RUB

ftmAlphaRub :: TransferMethod
ftmAlphaRub = tmAlpha RUB

ftmTinkoffRub :: TransferMethod
ftmTinkoffRub = tmTinkoff RUB

ftmQiwiRub :: TransferMethod
ftmQiwiRub = tmQiwi RUB

ftmPayPalRub :: TransferMethod
ftmPayPalRub = tmPayPal RUB

ftmPayPalUsd :: TransferMethod
ftmPayPalUsd = tmPayPal USD

ctmPzm :: TransferMethod
ctmPzm = CryptoTM PZM

ctmBtc :: TransferMethod
ctmBtc = CryptoTM BTC

ctmEth :: TransferMethod
ctmEth = CryptoTM ETH

ctmOuro :: TransferMethod
ctmOuro = CryptoTM OURO

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

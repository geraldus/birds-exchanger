{-# LANGUAGE OverloadedStrings #-}
module Utils.Form where

import           Local.Persist.Currency
import           Import.NoFoundation
import           Utils.Deposit                  ( depositRurMinCentsAmount
                                                , depositPzmMinCentsAmount
                                                )
import           Type.Money                     ( oneCoinCents )


amountIsValidC :: Currency -> Double -> Bool
amountIsValidC (CryptoC PZM) a =
    a * fromIntegral oneCoinCents >= fromIntegral depositPzmMinCentsAmount
amountIsValidC (FiatC RUR) a =
    a * fromIntegral oneCoinCents >= fromIntegral depositRurMinCentsAmount
amountIsValidC _ _ = False


currencyOptions :: [(Text, Currency)]
currencyOptions =
    [ ("Российский рубль ₽ (RUR)", FiatC RUR)
    , ("Криптовалюта Prizm (PZM)", CryptoC PZM)
    ]

currencyOptionListRaw :: [Option Currency]
currencyOptionListRaw =
    [ Option "Российский рубль ₽ (RUR)" (FiatC RUR)   "rur"
    , Option "Криптовалюта Prizm (PZM)" (CryptoC PZM) "pzm"
    ]

transferOptionsRaw :: [Option TransferMethod]
transferOptionsRaw =
    [ Option "перевод Prizm"                    ctmPzm        "pzm_tm"
    -- , Option "перевод Bitcoin" ctmBtc "btc_tm"
    -- , Option "перевод Etherium" ctmBtc "eth_tm"
    , Option "СберБанк - перевод на карту"      ftmSberRur    "rur_sber_tm"
    -- , Option "АльфаБанк - перевод на карту" ftmAlphaRur "rur_alpha_tm"
    , Option "Тинькофф Банк - перевод на карту" ftmTinkoffRur "rur_tinkoff_tm"
    , Option "Qiwi - перевод на кошелёк"        ftmQiwiRur    "rur_qiwi_tm"
    -- , Option "PayPal - перевод со счёта на счёт" ftmPayPalRur "rur_paypal_tm"
    -- , Option "PayPal - перевод со счёта на счёт" ftmPayPalUsd "usd_paypal_tm"
    ]
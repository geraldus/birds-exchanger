{-# LANGUAGE OverloadedStrings #-}
module Utils.Form where

import           Import.NoFoundation
import           Local.Params
import           Local.Persist.Currency
import           Type.Money             ( oneCoinCents )


amountIsValidC :: Currency -> Double -> Bool
amountIsValidC (CryptoC PZM) a =
    a * fromIntegral oneCoinCents >= fromIntegral depositPzmMinCentsAmount
amountIsValidC (FiatC RUR) a =
    a * fromIntegral oneCoinCents >= fromIntegral depositRurMinCentsAmount
amountIsValidC (CryptoC OUR) a =
    a * fromIntegral oneCoinCents >= fromIntegral depositOurMinCentsAmount
amountIsValidC _ _ = False


currencyOptions :: [(Text, Currency)]
currencyOptions =
    [ ("Российский рубль ₽ (RUB)", FiatC RUR)
    , ("Криптовалюта Prizm (PZM)", CryptoC PZM)
    , ("Криптовалюта Ouroboros (OUR)", CryptoC OUR)
    ]

currencyOptionListRaw :: [Option Currency]
currencyOptionListRaw =
    [ Option "Российский рубль ₽ (RUB)"     (FiatC RUR)   "rur"
    , Option "Криптовалюта Prizm (PZM)"     (CryptoC PZM) "pzm"
    , Option "Криптовалюта Ouroboros (OUR)" (CryptoC OUR) "our"
    ]

transferOptionsRaw :: [Option TransferMethod]
transferOptionsRaw =
    [ Option "перевод Prizm"                    ctmPzm        "pzm_tm"
    , Option "перевод Ouroboros"                ctmOur        "our_tm"
    -- , Option "перевод Bitcoin" ctmBtc "btc_tm"
    -- , Option "перевод Etherium" ctmBtc "eth_tm"
    , Option "СберБанк - перевод на карту"      ftmSberRur    "rur_sber_tm"
    -- , Option "АльфаБанк - перевод на карту" ftmAlphaRur "rur_alpha_tm"
    , Option "Тинькофф Банк - перевод на карту" ftmTinkoffRur "rur_tinkoff_tm"
    , Option "Qiwi - перевод на кошелёк"        ftmQiwiRur    "rur_qiwi_tm"
    -- , Option "PayPal - перевод со счёта на счёт" ftmPayPalRur "rur_paypal_tm"
    -- , Option "PayPal - перевод со счёта на счёт" ftmPayPalUsd "usd_paypal_tm"
    ]

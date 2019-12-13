{-# LANGUAGE OverloadedStrings #-}
module Utils.Form where

import           Import.NoFoundation
import           Local.Params
import           Local.Persist.Currency
import           Local.Persist.TransferMethod
import           Type.Money                   ( oneCoinCents )


amountIsValidC :: Currency -> Double -> Bool
amountIsValidC (CryptoC PZM) a =
    a * fromIntegral oneCoinCents >= fromIntegral depositPzmMinCentsAmount
amountIsValidC (FiatC RUB) a =
    a * fromIntegral oneCoinCents >= fromIntegral depositRubMinCentsAmount
amountIsValidC (CryptoC OURO) a =
    a * fromIntegral oneCoinCents >= fromIntegral depositOuroMinCentsAmount
amountIsValidC _ _ = False


currencyOptions :: [(Text, Currency)]
currencyOptions =
    [ ("Российский рубль ₽ (RUB)", FiatC RUB)
    , ("Криптовалюта Prizm (PZM)", CryptoC PZM)
    , ("Криптовалюта Ouroboros (OURO)", CryptoC OURO)
    ]

currencyOptionListRaw :: [Option Currency]
currencyOptionListRaw =
    [ Option "Российский рубль ₽ (RUB)"     (FiatC RUB)   "rub"
    , Option "Криптовалюта Prizm (PZM)"     (CryptoC PZM) "pzm"
    , Option "Криптовалюта Ouroboros (OURO)" (CryptoC OURO) "ouro"
    ]

transferOptionsRaw :: [Option TransferMethod]
transferOptionsRaw =
    [ Option "перевод Prizm"                    ctmPzm        "pzm_tm"
    , Option "перевод Ouroboros"                ctmOuro        "ouro_tm"
    -- , Option "перевод Bitcoin" ctmBtc "btc_tm"
    -- , Option "перевод Etherium" ctmBtc "eth_tm"
    , Option "СберБанк - перевод на карту"      ftmSberRub    "rub_sber_tm"
    -- , Option "АльфаБанк - перевод на карту" ftmAlphaRub "rub_alpha_tm"
    , Option "Тинькофф Банк - перевод на карту" ftmTinkoffRub "rub_tinkoff_tm"
    , Option "Qiwi - перевод на кошелёк"        ftmQiwiRub    "rub_qiwi_tm"
    -- , Option "PayPal - перевод со счёта на счёт" ftmPayPalRub "rub_paypal_tm"
    -- , Option "PayPal - перевод со счёта на счёт" ftmPayPalUsd "usd_paypal_tm"
    ]

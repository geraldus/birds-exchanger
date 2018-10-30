{-# LANGUAGE OverloadedStrings #-}
module Form.Profile.Deposit where


import           Import
import           Local.Persist.Currency

import           Text.Blaze.Html.Renderer.Text (renderHtml)


amountIsValidC :: Currency -> Double -> Bool
amountIsValidC (CryptoC PZM) a = a * fromIntegral oneCent * depositPzmRurRatio >= fromIntegral depositMinCentAmount
amountIsValidC (FiatC RUR) a = a * fromIntegral oneCent >= fromIntegral depositMinCentAmount
amountIsValidC _ _ = False

doubleToCents :: Double -> Int
doubleToCents x = truncate $ x * fromIntegral oneCent

selectOpposite' :: Currency -> Currency
selectOpposite' (FiatC RUR)   = CryptoC PZM
selectOpposite' (CryptoC PZM) = FiatC RUR

selectMethod' :: Currency -> PaymentMethod
selectMethod' (FiatC RUR)   = FiatPM Card2CardFPM RUR
selectMethod' (CryptoC PZM) = CryptoPM PZM

selectFee :: Currency -> Fee
selectFee (FiatC RUR) = depositFeeRur
selectFee (CryptoC PZM) = depositFeePzm

calcFeeCents :: Fee -> Int -> Int
calcFeeCents (Percent p) c = ceiling $ fromIntegral c * p / 100

--         amountCents   = doubleToCents <$> paymentCurrencyAmountRes
--         targetCurrency = selectOpposite' <$> paymentCurrencyRes
--         paymentMethod = selectMethod' <$> paymentCurrencyRes

depositForm :: Form DepositRequestFD
depositForm extra = do
    (paymentCurrencyRes, paymentCurrencyView) <- mreq currencySelect "" Nothing
    (paymentCurrencyAmountRes, paymentCurrencyAmountView) <- mreq doubleField "" Nothing
    (targetCurrencyRes, targetCurrencyView) <- mreq (selectFieldList currencyOptions) "" Nothing
    let amountIsValidRes = amountIsValidC <$> paymentCurrencyRes <*> paymentCurrencyAmountRes
        amountCentsRes   = doubleToCents <$> paymentCurrencyAmountRes
        paymentMethodRes = selectMethod' <$> paymentCurrencyRes
        depReqRes = DepositRequestFD
                        <$> paymentCurrencyRes
                        <*> paymentMethodRes
                        <*> amountCentsRes
                        <*> targetCurrencyRes
        formResult = case amountIsValidRes of
            FormSuccess True -> depReqRes
            FormSuccess False -> FormFailure $ [
                    toStrict $ renderHtml [shamlet|
                        $newline never
                        Минимальная сумма пополнения #
                        \#{show (round (fromIntegral depositMinCentAmount / fromIntegral oneCent))} ₽ / #
                        \#{show (fromIntegral depositMinCentAmount * (depositRurPzmRatio / fromIntegral oneCent))} #
                        \PZM|]
                ]
            FormFailure es -> FormFailure es
            FormMissing -> FormMissing
    let widget = do
            inCurrencyId <- newIdent
            inTargetCurrencyId <- newIdent
            [whamlet|
                #{extra}
                <div ##{inCurrencyId}>
                    ^{fvInput paymentCurrencyView}
                <div ##{inTargetCurrencyId}>
                    ^{fvInput targetCurrencyView}
                <div>
                    ^{fvInput paymentCurrencyAmountView}
                |]
                -- ^ {fvInput paymentMethodView}
    return (formResult, widget)


data DepositRequestFD = DepositRequestFD
    { depReqCurrency       :: Currency
    , depReqPaymentMethod  :: PaymentMethod
    , depReqCentsAmount    :: Int
    -- , depReqCentsExpectedFee :: Int
    -- , depReqTransactionCode :: Text
    , depReqTargetCurrency :: Currency
    -- , depReqExpectedConversionRatio :: Double
    -- , depReqCentsExpectedIncome :: Amount
    }


currencySelect :: Field (HandlerFor App) Currency
currencySelect = selectFieldList currencyOptions

currencyOptions :: [(Text, Currency)]
currencyOptions =
    [ ("₽ российский рубль", FiatC RUR)
    , ("PZM криптовалюта Prizm", CryptoC PZM) ]

paymentMethodOptions :: [(Text, PaymentMethod)]
paymentMethodOptions =
    [ ("Перевод с карты на карту, ₽ российский рубль", FiatPM Card2CardFPM RUR)
    , ("Перевод криптовалюты, PZM криптовалюта Prizm", CryptoPM PZM) ]

typedCurrencyOptions :: [(CurrencyType, Currency)]
typedCurrencyOptions =
    [ (FiatT, FiatC RUR)
    , (CryptoT, CryptoC PZM)
    ]

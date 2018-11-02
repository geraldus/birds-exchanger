{-# LANGUAGE OverloadedStrings #-}
module Form.Profile.Deposit where


import           Import
import           Local.Persist.Currency
import           Utils.Deposit

import           Text.Blaze.Html.Renderer.Text (renderHtml)


amountIsValidC :: Currency -> Double -> Bool
amountIsValidC (CryptoC PZM) a = a * fromIntegral oneCent * depositPzmRurRatio >= fromIntegral depositMinCentAmount
amountIsValidC (FiatC RUR) a = a * fromIntegral oneCent >= fromIntegral depositMinCentAmount
amountIsValidC _ _ = False

selectOpposite' :: Currency -> Currency
selectOpposite' (FiatC RUR)   = CryptoC PZM
selectOpposite' (CryptoC PZM) = FiatC RUR

selectMethod' :: Currency -> PaymentMethod
selectMethod' (FiatC RUR)   = FiatPM Card2CardFPM RUR
selectMethod' (CryptoC PZM) = CryptoPM PZM


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
        matchingFee = selectFee <$> paymentCurrencyRes
        expectedFee = calcFeeCents <$> matchingFee <*> amountCentsRes
        expectedRatio = selectRatio' <$> paymentCurrencyRes <*> targetCurrencyRes
        depReqRes = DepositRequestFD
                        <$> paymentCurrencyRes
                        <*> paymentMethodRes
                        <*> amountCentsRes
                        <*> expectedFee
                        <*> targetCurrencyRes
                        <*> expectedRatio
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
    , depReqCentsExpectedFee :: Int
    , depReqTargetCurrency :: Currency
    , depReqExpectedConversionRatio :: Double
    }


paymentMethodOptions :: [(Text, PaymentMethod)]
paymentMethodOptions =
    [ ("Перевод с карты на карту, ₽ российский рубль", FiatPM Card2CardFPM RUR)
    , ("Перевод криптовалюты, PZM криптовалюта Prizm", CryptoPM PZM) ]

typedCurrencyOptions :: [(CurrencyType, Currency)]
typedCurrencyOptions =
    [ (FiatT, FiatC RUR)
    , (CryptoT, CryptoC PZM)
    ]

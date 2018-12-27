{-# LANGUAGE OverloadedStrings #-}
module Form.Profile.Deposit where


import           Import
import           Local.Persist.Currency
import           Utils.Deposit

import           Text.Blaze.Html.Renderer.Text (renderHtml)


amountIsValidC :: Currency -> Double -> Bool
amountIsValidC (CryptoC PZM) a = a * fromIntegral oneCoinCents * depositPzmRurRatio >= fromIntegral depositMinCentAmount
amountIsValidC (FiatC RUR) a = a * fromIntegral oneCoinCents >= fromIntegral depositMinCentAmount
amountIsValidC _ _ = False

selectOpposite' :: Currency -> Currency
selectOpposite' (FiatC RUR)   = CryptoC PZM
selectOpposite' (CryptoC PZM) = FiatC RUR

-- selectMethod' :: Currency -> PaymentMethod
-- selectMethod' (FiatC RUR)   = FiatPM Card2CardFPM RUR
-- selectMethod' (CryptoC PZM) = CryptoPM PZM


--         amountCents   = doubleToCents <$> paymentCurrencyAmountRes
--         targetCurrency = selectOpposite' <$> paymentCurrencyRes
--         paymentMethod = selectMethod' <$> paymentCurrencyRes

moneyInput :: Text
           -> Text
           -> MForm (HandlerFor App) (FormResult Money, Widget)
moneyInput aid cid = do
    (amt, av) <- mreq doubleField (fsBs4WithId aid) Nothing
    (cur, cv) <- mreq currencySelect (fsBs4WithId cid) Nothing
    let wid = [whamlet|
                <div .form-row>
                    <div .col-12.col-md-4>
                        <label for="#{aid}">кол-во
                        ^{fvInput av}
                    <div .col-12.col-md-8>
                        <label for="#{cid}">валюта
                        ^{fvInput cv}
                |]
    return  (Money <$> (doubleToCents <$> amt) <*> cur, wid)


withdrawalForm :: Form WithdrawalM
withdrawalForm extra = do
    adrid <- newIdent
    amtid <- newIdent
    curid <- newIdent
    (moneyRes, moneyWid) <- moneyInput amtid curid
    (addressRes, addressView) <- mreq textField (fsBs4WithId adrid) Nothing
    let widget = [whamlet|
            #{extra}
            <div .form-row>
                <p .h4>Вывожу
            ^{moneyWid}
            <div .form-row>
                <label for="#{adrid}">Карта или номер кошелька
                ^{fvInput addressView}
            |]
        res = WithdrawalM <$> moneyRes <*> addressRes
    return (res, widget)


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
                        \#{show (round (fromIntegral depositMinCentAmount / fromIntegral oneCoinCents))} ₽ / #
                        \#{show (fromIntegral depositMinCentAmount * (depositRurPzmRatio / fromIntegral oneCoinCents))} #
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
    { depReqCurrency                :: Currency
    , depReqPaymentMethod           :: PaymentMethod
    , depReqCentsAmount             :: Int
    , depReqCentsExpectedFee        :: Int
    , depReqTargetCurrency          :: Currency
    , depReqExpectedConversionRatio :: Double
    }


data Money = Money Int Currency deriving Show

data WithdrawalM = WithdrawalM Money Text deriving Show

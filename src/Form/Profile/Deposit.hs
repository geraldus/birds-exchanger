{-# LANGUAGE OverloadedStrings #-}
module Form.Profile.Deposit where


import           Import
import           Local.Persist.Currency
import           Type.Money                    ( oneCoinCents )
import           Utils.Deposit
import           Utils.Form
import           Utils.Money                   ( truncCoins2Cents )

import           Text.Blaze.Html.Renderer.Text ( renderHtml )
import           Text.Julius                   ( RawJS (..) )



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


depositForm :: Text -> Form DepositRequestFD
depositForm formId extra = do
    cid <- newIdent
    tid <- newIdent
    aid <- newIdent
    (paymentCurrencyRes, paymentCurrencyView) <- mreq currencySelect (fsBs4WithId cid) Nothing
    (paymentAmountRes, paymentAmountView) <- mreq
        doubleField
        ( fsAddClasses
            ( fsAddPlaceholder
                ( fsBs4WithId aid ) "укажите сумму" )
            [ "form-control-lg" ] )
        Nothing
    (transferMethodRes, transferMethodView) <- mreq transferMethodSelect (fsBs4WithId tid) Nothing
    -- (targetCurrencyRes, _) <- mreq
    --     ( selectFieldList currencyOptions )
    --     fsBs4
    --     Nothing
    let amountIsValidRes = amountIsValidC <$> paymentCurrencyRes <*> paymentAmountRes
        amountCentsRes   = truncCoins2Cents <$> paymentAmountRes
        matchingFee = selectFee <$> paymentCurrencyRes
        expectedFee = calcFeeCents <$> matchingFee <*> amountCentsRes
        expectedRatio = selectRatio' <$> paymentCurrencyRes <*> paymentCurrencyRes-- targetCurrencyRes
        depReqRes = DepositRequestFD
                        <$> paymentCurrencyRes
                        <*> transferMethodRes
                        <*> amountCentsRes
                        <*> expectedFee
                        <*> paymentCurrencyRes
                        -- ^ for now no conversion; no instant exchange
                        -- <*> targetCurrencyRes
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
    let isValidTransferMethod = isJust . fvErrors $ transferMethodView
    let widget = do
            inCurrencyId <- newIdent
            inTargetCurrencyId <- newIdent
            $(widgetFile "form/client-deposit")
    return (formResult, widget)


data DepositRequestFD = DepositRequestFD
    { depReqCurrency                :: Currency
    , depReqTransferMethod          :: TransferMethod
    , depReqCentsAmount             :: Int
    , depReqCentsExpectedFee        :: Int
    , depReqTargetCurrency          :: Currency
    , depReqExpectedConversionRatio :: Double
    }

data WithdrawalM = WithdrawalM Money Text deriving Show

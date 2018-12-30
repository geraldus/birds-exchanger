{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Form.Profile.Deposit where


import           Import
import           Local.Persist.Currency
import           Utils.Deposit
import           Utils.Form
import           Utils.Money                   ( truncCoins2Cents )

import           Text.Blaze.Html.Renderer.Text ( renderHtml )
import           Text.Julius                   ( RawJS (..) )


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
                ( fsBs4WithId aid ) "0.00" )
            [ "form-control-lg", "text-center" ] )
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
        expectedRatio = selectRatio <$> paymentCurrencyRes <*> paymentCurrencyRes-- targetCurrencyRes
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
            FormSuccess False -> FormFailure [
                    toStrict $ renderHtml [shamlet|
                        $newline never
                        Минимальная сумма пополнения #
                        \#{cents2dblT depositRurMinCentsAmount} ₽ / #
                        \#{cents2dblT depositPzmMinCentsAmount} #
                        \PZM|]
                ]
            FormFailure es -> FormFailure es
            FormMissing -> FormMissing
    -- let isValidTransferMethod = isJust . fvErrors $ transferMethodView
    let widget = do
            inCurrencyId <- newIdent
            -- inTargetCurrencyId <- newIdent
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

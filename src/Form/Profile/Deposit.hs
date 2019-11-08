{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Form.Profile.Deposit where


import           Import
import           Local.Params                  ( currencyDefaultMinimalDeposit )
import           Local.Persist.Currency
import           Utils.Deposit
import           Utils.Form
import           Utils.Money

import           Text.Blaze.Html.Renderer.Text ( renderHtml )
import           Text.Julius                   ( RawJS (..) )


depositForm :: Text -> Form DepositRequestFD
depositForm formId extra = do
    cid <- newIdent
    tid <- newIdent
    aid <- newIdent
    renderMessage <- getMessageRender
    (paymentCurrencyRes, paymentCurrencyView) <- mreq currencySelect (fsBs4WithId cid) Nothing
    (paymentAmountRes, paymentAmountView) <- mreq
        doubleField
        ( fsAddClasses
            ( fsAddPlaceholder
                ( fsBs4WithId aid ) "0.00" )
            [ "form-control-lg", "text-center" ] )
        Nothing
    (transferMethodRes, transferMethodView) <- mreq transferMethodSelect (fsBs4WithId tid) Nothing
    let amountIsValidRes = amountIsValidC <$> paymentCurrencyRes <*> paymentAmountRes
        amountCentsRes   = truncCoins2Cents <$> paymentAmountRes
        matchingFee = selectDepositFee <$> paymentCurrencyRes
        expectedFee = calcFeeCents <$> matchingFee <*> amountCentsRes
        -- expectedRatio = selectRatio <$> paymentCurrencyRes <*> paymentCurrencyRes
        depReqRes = DepositRequestFD
                        <$> paymentCurrencyRes
                        <*> transferMethodRes
                        <*> amountCentsRes
                        <*> expectedFee
                        <*> paymentCurrencyRes
                        -- <*> expectedRatio
    let mayPaymentCurrency = maybeSuccess paymentCurrencyRes
        formResult = case amountIsValidRes of
            FormSuccess True -> depReqRes
            FormSuccess False ->
                let limits = fromMaybeMinAmountDesc mayPaymentCurrency
                in FormFailure [
                    toStrict $ renderHtml [shamlet|
                        $newline never
                        #{renderMessage MsgFormMessageErrorInvalidMinimalAmount} #
                        \#{limits}|]
                ]
            FormFailure es -> FormFailure es
            FormMissing -> FormMissing
    -- TODO: FIXME: Check if Transfer Method is valid
    -- let isValidTransferMethod = isJust . fvErrors $ transferMethodView
    let widget = do
            inCurrencyId <- newIdent
            $(widgetFile "form/client-deposit")
    return (formResult, widget)

fromMaybeMinAmountDesc :: Maybe Currency -> Html
fromMaybeMinAmountDesc mc = [shamlet|#{messages}|]
    where
        messages = intercalate " / " cs'
        cs' = flip map cs $ \c -> [shamlet|#{cents2dblT (currencyDefaultMinimalDeposit c)} #{currSign c}|]
        cs = fromMaybeMinAmountCurrencies mc

fromMaybeMinAmountCurrencies :: Maybe Currency -> [ Currency ]
fromMaybeMinAmountCurrencies (Just c) = [ c ]
fromMaybeMinAmountCurrencies _ = [ pzmC, ourC, rurC ]

maybeSuccess :: FormResult a -> Maybe a
maybeSuccess (FormSuccess x) = Just x
maybeSuccess _               = Nothing


data DepositRequestFD = DepositRequestFD
    { depReqCurrency                :: Currency
    , depReqTransferMethod          :: TransferMethod
    , depReqCentsAmount             :: Int
    , depReqCentsExpectedFee        :: Int
    , depReqTargetCurrency          :: Currency
    -- , depReqExpectedConversionRatio :: Double
    }

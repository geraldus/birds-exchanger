{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Form.Profile.Deposit where


import           Import
import           Local.Params                  ( currencyDefaultMinimalDeposit )
import           Local.Persist.Currency
import           Local.Persist.TransferMethod  ( TransferMethod )
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
    messageRender  <- getMessageRender
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
        depReqRes = DepositRequestFD
                        <$> paymentCurrencyRes
                        <*> transferMethodRes
                        <*> amountCentsRes
                        <*> expectedFee
                        <*> paymentCurrencyRes
    let mayPaymentCurrency = maybeSuccess paymentCurrencyRes
        formResult = case amountIsValidRes of
            FormSuccess True -> depReqRes
            FormSuccess False ->
                let limits = fromMaybeMinAmountDesc mayPaymentCurrency
                in FormFailure [
                    toStrict $ renderHtml [shamlet|
                        $newline never
                        #{messageRender MsgFormMessageErrorInvalidMinimalAmount} #
                        \#{limits}|]
                ]
            FormFailure es -> FormFailure es
            FormMissing -> FormMissing
    let widget = do
            inCurrencyId <- newIdent
            $(widgetFile "form/client-deposit")
    return (formResult, widget)

fromMaybeMinAmountDesc :: Maybe Currency -> Html
fromMaybeMinAmountDesc mc = [shamlet|#{messages}|]
    where
        messages = intercalate " / " cs'
        cs' = flip map cs $ \c ->
            let amountCents = (cents2dblT . currencyDefaultMinimalDeposit) c
                symbol = currencySymbol c
            in [shamlet|#{amountCents}&nbsp;#{symbol}|]
        cs = fromMaybeMinAmountCurrencies mc

fromMaybeMinAmountCurrencies :: Maybe Currency -> [ Currency ]
fromMaybeMinAmountCurrencies (Just c) = [ c ]
fromMaybeMinAmountCurrencies _        = [ pzmC, ouroC, rubC ]

maybeSuccess :: FormResult a -> Maybe a
maybeSuccess (FormSuccess x) = Just x
maybeSuccess _               = Nothing


data DepositRequestFD = DepositRequestFD
    { depReqCurrency         :: Currency
    , depReqTransferMethod   :: TransferMethod
    , depReqCentsAmount      :: Int
    , depReqCentsExpectedFee :: Int
    , depReqTargetCurrency   :: Currency
    }

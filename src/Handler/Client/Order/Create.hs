{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Client.Order.Create where

import           Import

import           Form.Exchanger.Order
import           Local.Params                    ( defaultExchangeFee )
import           Local.Persist.Currency
import           Local.Persist.Exchange
import           Utils.Database.Operations
import           Utils.Money


data ProcessForm
    = ProcessFormNoData
    | ProcessFormErrors [ LabeledError ]
    | ProcessFormSuccess
            AmountCents Currency UTCTime (UserWallet -> OrderCheck)

postExchangeOrderCreateR :: Handler Html
postExchangeOrderCreateR = do
    client <- requireClientId
    rid <- newIdent
    wid <- newIdent
    ((res, _), _) <- runFormPost $ createOrderForm wid rid
        ExchangePzmRur -- This is not actually used here, will be
        -- valueable when rendering form widget takes place
    proceessResult <- case res of
        FormMissing    -> return ProcessFormNoData
        FormFailure es -> return $
                ProcessFormErrors $ zip (repeat "form") es
        FormSuccess od -> do
            let act = action od
                r = ratio od
                formPair = pair od
                d = giveTake act formPair (flipPair formPair)
            let a = case act of
                        EAGive -> amount od
                        EAReceive ->
                            let k = pairRatioByNormalizedRatio formPair r
                            in multiplyCents k (amount od)
            mr <- getMessageRender
            t <- liftIO getCurrentTime
            return $ processNewOrder client d a r t mr
    case proceessResult of
        ProcessFormErrors es -> do
            -- TODO: FIXME: Add JSON response capabilities
            let msg = [shamlet|
                    $forall (_, message) <- es
                        <div>#{message}|]
            setMessage msg
        ProcessFormSuccess a c t checkOrder ->
            runDB (saveAndExecuteOrder client a c t checkOrder) >> return ()
        _ -> return ()
    redirect HomeR



processNewOrder
    :: UserId
    -> ExchangePair
    -> AmountCents
    -> NormalizedRatio
    -> UTCTime
    -> (AppMessage -> Text)
    -> ProcessForm
processNewOrder client epair a r t mr =
    let er a' r' w' = orderCreateRenderFormErrors a' r' w' mr
        chk w = checkOrderData epair a r client w t er
        c = fst . unPairCurrency $ epair
    in ProcessFormSuccess a c t chk

checkOrderData
    :: ExchangePair
    -> AmountCents
    -> NormalizedRatio
    -> UserId
    -> UserWallet
    -> UTCTime
    -> (Int -> Double -> UserWallet -> [ LabeledError ])
    -> OrderCheck
checkOrderData
        exchangeDirection
        amount
        ratio
        client
        wallet
        time
        renderErrors =
    if amount <= 0 || ratio <= 0 || amount > userWalletAmountCents wallet
        then OrderCheckErrors (renderErrors amount ratio wallet)
        else
            let r = pairRatioByNormalizedRatio exchangeDirection ratio
                -- Ratio Multiplicator
                -- Ratio within form data is in normalized state, convert it
                -- to direct multiplication value
                t = multiplyCents r amount
                -- Target Amount
                -- Amount cents required for full order exchange
                f = calcFeeCents defaultExchangeFee t
                -- Expected fee, shown for user when order was created
                -- d = giveTake action pair (flipPair epair)
                d = exchangeDirection
                -- Correct Exchange Direction
                -- Pair within form data doesn't represent correct exchange
                -- direction, it must be interpreted depending on 'action', e.g.
                -- give action -- same direction, take action -- flipped.
            in OrderCheckSuccess $
                    mkNewOrderData client amount ratio d f time

orderCreateRenderFormErrors
    :: AmountCents
    -> NormalizedRatio
    -> UserWallet
    -> (AppMessage -> Text)
    -> [ LabeledError ]
orderCreateRenderFormErrors a r w m =
    let amount = mkError (a <= 0) "amount" (m MsgPositiveValueRequired)
        ratio  = mkError (r <= 0) "ratio" (m MsgPositiveValueRequired)
        balance = mkError
                (userWalletAmountCents w < a) "balance" (m MsgNotEnoughFunds)
    in amount <> ratio <> balance
  where mkError cond name e = [ (name, e) | cond ]


giveTake :: ExchangeAction -> p -> p -> p
giveTake a giveChoise takeChoise = if a == EAGive
    then giveChoise else takeChoise

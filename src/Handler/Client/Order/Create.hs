{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Client.Order.Create where

import           Import

import           Form.Exchanger.Order
import           Handler.Client.Paramining ( scheduleParaminingMapAccrual )
import           Local.Params              ( defaultExchangeFee )
import           Local.Persist.Currency
import           Local.Persist.Exchange
import           Utils.Database.Operations
import           Utils.Money


data ProcessForm
    = ProcessFormNoData
    | ProcessFormErrors [ LabeledError ]
    | ProcessFormSuccess
            AmountCents Currency UTCTime ExchangePair (UserWallet -> OrderCheck)

postExchangeOrderCreateR :: Handler Html
postExchangeOrderCreateR = do
    client <- requireClientId
    rid <- newIdent
    wid <- newIdent
    ((formResult, _), _) <- runFormPost $ createOrderForm wid rid
        ExchangePzmRur -- This is not actually used here, will be
        -- valueable when rendering form widget takes place
    processResult <- case formResult of
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
    pair' <- case processResult of
        ProcessFormErrors es -> do
            -- TODO: FIXME: Add JSON response capabilities
            let msg = [shamlet|
                    $forall (_, message) <- es
                        <div>#{message}|]
            setMessage msg
            return ExchangeOurRur
        ProcessFormSuccess a c t p checkOrder -> do
            res <- runDB $
                 saveAndExecuteOrder client a c t checkOrder
            case res of
                Insertion _ paraMap _ -> scheduleParaminingMapAccrual paraMap
                _                     -> return ()
            return . flipPair . defPairDir $ p
        _ -> return ExchangeOurRur
    let (c1', c2') = unPairCurrency pair'
        c1 = toLower $ currencyCodeT c1'
        c2 = toLower $ currencyCodeT c2'
    renderUrl <- getUrlRenderParams
    let url = renderUrl HomeR [("from", c1), ("to", c2)]
    redirect url


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
    in ProcessFormSuccess a c t epair chk

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
        amountCents
        orderRatio
        client
        wallet
        time
        renderErrors =
    if amountCents <= 0
            || orderRatio <= 0
            || amountCents > userWalletAmountCents wallet
        then OrderCheckErrors (renderErrors amountCents orderRatio wallet)
        else
            let r = pairRatioByNormalizedRatio exchangeDirection orderRatio
                -- Ratio Multiplicator
                -- Ratio within form data is in normalized state, convert it
                -- to direct multiplication value
                t = multiplyCents r amountCents
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
                    mkNewOrderData client amountCents orderRatio d f time

orderCreateRenderFormErrors
    :: AmountCents
    -> NormalizedRatio
    -> UserWallet
    -> (AppMessage -> Text)
    -> [ LabeledError ]
orderCreateRenderFormErrors a r w m =
    let amountError = mkError (
            a <= 0) "amount" (m MsgFormMessageErrorPositiveValueRequired)
        ratioError  = mkError
            (r <= 0) "ratio" (m MsgFormMessageErrorPositiveValueRequired)
        balanceError = mkError
            (userWalletAmountCents w < a)
            "balance"
            (m MsgFormMessageErrorNotEnoughFunds)
    in amountError <> ratioError <> balanceError
  where mkError condition name e = [ (name, e) | condition ]


giveTake :: ExchangeAction -> p -> p -> p
giveTake a giveChoise takeChoise = if a == EAGive
    then giveChoise else takeChoise

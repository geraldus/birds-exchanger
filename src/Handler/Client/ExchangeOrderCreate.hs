{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}
module Handler.Client.ExchangeOrderCreate where


import           Form.Exchanger.Order
import           Import
import           Local.Persist.Currency
import           Local.Persist.Deposit
import           Local.Persist.ExchangeOrder
import           Local.Persist.Wallet
import           Utils.Deposit               ( calcFeeCents )


postExchangeOrderCreateR :: Handler Html
postExchangeOrderCreateR = do
    clientData <- requireClient
    ((res, widget), enctype) <- runFormPost formCreateExchageOrder
    case res of
        FormMissing -> invalidArgs ["Данные не получены"]
        FormFailure es -> do
            setMessage [shamlet|
                <p .h5>Ошибка создания ордера на обмен
                <ul>
                    $forall e <- es
                        <li>#{e}
                |]
            redirect HomeR
        FormSuccess ExchangeOrderData{..} -> do
            let (Entity userId _, wallets) = clientData
            outWallet <- getMatchingWallet userId orderDataCurrencyOut wallets
            inWallet  <- getMatchingWallet userId orderDataCurrencyIn wallets
            if (userWalletAmountCents $ entityVal outWallet) < orderDataAmountCents
                then defaultLayout $ do
                    setTitle [shamlet|
                        <p>Для создания ордера недостаточно средств на счёте
                        <p>Пополните баланс|]
                    redirect HomeR
                else do
                    time <- liftIO getCurrentTime
                    _ <- runDB $ do
                        let outWalletId = entityKey outWallet
                            inWalletId = entityKey inWallet
                        wtrId <- insert $ WalletTransactionReason outWalletId
                        wbtId <- insert $ WalletBalanceTransaction
                            outWalletId
                            (ExchangeFreeze orderDataAmountCents)
                            wtrId
                            (userWalletAmountCents . entityVal $ outWallet)
                        update outWalletId [UserWalletAmountCents -=. orderDataAmountCents]
                        let order = ExchangeOrder
                                userId
                                inWalletId
                                outWalletId
                                orderDataCurrencyOut
                                orderDataCurrencyIn
                                orderDataAmountCents
                                orderDataRatio
                                orderDataExpectedFeeCents
                                time
                                (Created time)
                                True
                                wtrId
                        orderId <- insert order
                        -- TODO: FIXME: Check if order can be executed already
                        instantOrderExecution (Entity orderId order)
                    defaultLayout $ do
                        setMessage [shamlet|<p .h5>Ордер успешно создан|]
                        redirect HomeR



getMatchingWallet :: UserId -> Currency -> [Entity UserWallet] -> Handler (Entity UserWallet)
getMatchingWallet userId currency wallets = do
    let mwallet = find (\(Entity _ UserWallet{..}) -> userWalletCurrency == currency) wallets
    case mwallet of
        Nothing     -> getOrCreateWallet userId currency
        Just wallet -> return wallet


instantOrderExecution :: Entity ExchangeOrder -> SqlPersistT Handler (Entity ExchangeOrder, [Entity ExchangeOrderExecution])
instantOrderExecution orderEntity = do
    let (Entity orderId order) = orderEntity
    let userId = exchangeOrderUserId order
        userCurrencyOut = exchangeOrderCurrencyOut order
        userCurrencyIn = exchangeOrderCurrencyIn order
        userWalletInId = exchangeOrderWalletInId order
        userWalletOutId = exchangeOrderWalletOutId order
        userAmountOutCents = exchangeOrderAmountCents order
        userRatio  = exchangeOrderRatio order
        targetRatio = 1 / userRatio
    mayOrder <- selectFirst
        [ ExchangeOrderCurrencyOut ==. userCurrencyIn
        , ExchangeOrderRatio <=. targetRatio ]
        [ Asc ExchangeOrderRatio
        , Asc ExchangeOrderCreated ]
    case mayOrder of
        Nothing -> return (orderEntity, [])
        Just match -> do
            -- We have an exchange possibility
            let (Entity  matchId matchOrder) = match
            let availAmt = exchangeOrderAmountCents . entityVal $ match
                -- TODO:  FIXME:  Check several times truncation and rounding EVERYWHERE!
                targetInAmt = fromIntegral userAmountOutCents * userRatio
            -- 1 Record transaction reason for each wallet
            let matchUserId = exchangeOrderUserId matchOrder
                matchCurrencyIn = exchangeOrderCurrencyIn matchOrder
                matchWalletInId = exchangeOrderWalletInId matchOrder
                matchRatio = exchangeOrderRatio matchOrder
            (Entity _ userInWallet) <- getOrCreateWalletDB userId userCurrencyIn
            (Entity _ matchInWallet) <- getOrCreateWalletDB matchUserId matchCurrencyIn
            userWalletTRId <- insert $ WalletTransactionReason userWalletInId
            matchWalletTRId <- insert $ WalletTransactionReason matchWalletInId
            -- 2 update wallet balances
            -- TODO: FIXME: TRUNCATE?
            let userAmountInCents = min (truncate targetInAmt) availAmt
                userFeeCents = calcFeeCents defaultExchangeFee userAmountInCents
                userFinalIncomeCents = userAmountInCents - userFeeCents
            let matchAmountInCents = truncate $ fromIntegral userAmountInCents * matchRatio
                matchFeeCents = calcFeeCents defaultExchangeFee matchAmountInCents
                matchFinalIncomeCents = matchAmountInCents - matchFeeCents
            -- 2.1 user balance income
            userWalletBTId <- insert $ WalletBalanceTransaction
                userWalletInId
                (ExchangeExchange userFinalIncomeCents)
                userWalletTRId
                (userWalletAmountCents userInWallet)
            update userWalletInId [UserWalletAmountCents +=. userFinalIncomeCents]
            -- 2.2 match balance id
            matchWalletBTId <- insert $ WalletBalanceTransaction
                matchWalletInId
                (ExchangeExchange matchFinalIncomeCents)
                matchWalletTRId
                (userWalletAmountCents matchInWallet)
            update matchWalletInId [UserWalletAmountCents +=. matchFinalIncomeCents]
            -- 3. Record fees
            time <- liftIO getCurrentTime
            userFeeInId <- insert $ FeeIncome userCurrencyIn userFeeCents userWalletTRId time
            matchFeeInId <- insert $ FeeIncome matchCurrencyIn matchFeeCents matchWalletTRId time
            -- 4. Record execution order statuses and events
            let userOrderIsCompleted = fromIntegral availAmt >= targetInAmt
                matchOrderIsCompleted = fromIntegral availAmt <= targetInAmt
            -- 4.1 Before recursively execute user order it's better
            -- to record match order execution and update
            -- its status
            matchExecId <- insert $ ExchangeOrderExecution
                matchId
                time
                userWalletTRId -- out wallet reason
                matchWalletTRId -- in wallet reason
                matchOrderIsCompleted
                userAmountInCents
                matchFinalIncomeCents
                matchFeeCents
            let matchOrderStatus = if matchOrderIsCompleted
                then Executed time
                else PartiallyExecuted time userAmountInCents
                -- здесь userAmountIn потому что в ордере
                -- записано кол-во отдаваемой валюты,
                -- а не получаемой
            update
                matchId
                [ ExchangeOrderStatus   =. matchOrderStatus
                , ExchangeOrderIsActive =.  not matchOrderIsCompleted ]
            -- 4.2 User order
            userOrderExecutionId <- insert $ ExchangeOrderExecution
                orderId
                time
                matchWalletTRId -- out wallet reason
                userWalletTRId -- in wallet reason
                userOrderIsCompleted
                matchAmountInCents
                userFinalIncomeCents
                userFeeCents
            let userOrderStatus = if userOrderIsCompleted
                then Executed time
                else PartiallyExecuted time matchAmountInCents
                -- здесь matchAmountIn потому что в ордере
                -- записано кол-во отдаваемой валюты,
                -- а не получаемой
            update
                orderId
                [ ExchangeOrderStatus =. userOrderStatus
                , ExchangeOrderIsActive =. not userOrderIsCompleted ]
            rest <- when (not userOrderIsCompleted) $ error "Recurse"
            return $ error "merge all current data with rest recursive"
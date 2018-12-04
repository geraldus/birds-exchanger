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

-- getFirstMatchingOrder :: Maybe
-- поиск ордера

-- executeOrderStep :: ()
-- выполняет одну экзекуцию

-- executeOrder :: OrderStatus
    -- Логика будет такой:
    -- Ищем первый подходящий ордер
    --   валюта та, что нужна
    --   сортируем по коэффициенту от меньшего к большему
    --   сортируем по дате создания от меньшего к большему
    -- Если нет -- заканчиваем
    -- Если есть -- исполняем
    --  проверяем, есть ли нужное количество монет
    --  снимаем сколько можно
    --  если найденный ордер закончен -- финализируем его
    --  если новый ордер закончен -- финализируем его и сообщаем
    --  если новый ордер не закончен -- повторяем процедуру


{-


PARTIAL EXECUTION! Amount should be TOTAL!  SO...
Sum previous value if exists!


-}



instantOrderExecution :: Entity ExchangeOrder -> SqlPersistT Handler (ExchangeOrderStatus, [Entity ExchangeOrderExecution])
instantOrderExecution = orderExecutionStep

orderExecutionStep
    :: Entity ExchangeOrder
    -> SqlPersistT Handler (ExchangeOrderStatus, [Entity ExchangeOrderExecution])
    -- ^ Bool indicates is order fully executed
orderExecutionStep userOrder = do
    let (Entity orderId order) = userOrder
    let userId = exchangeOrderUserId order
        userCurrencyIn = exchangeOrderCurrencyIn order
        userCurrencyOut = exchangeOrderCurrencyOut order
        userWalletInId = exchangeOrderWalletInId order
        userAmountOutCents = exchangeOrderAmountCents order
        userRatio  = exchangeOrderRatio order
        targetRatio = 1 / userRatio
    -- TODO: FIXME: Prevent archieved and cancelled orders to be selected!!
    mayOrder <- selectFirst
        [ ExchangeOrderIsActive ==. True
        , ExchangeOrderCurrencyOut ==. userCurrencyIn
        , ExchangeOrderRatio <=. targetRatio ]
        [ Asc ExchangeOrderRatio
        , Asc ExchangeOrderCreated ]
    case mayOrder of
        Nothing -> return (exchangeOrderStatus order, [])
        Just match -> do
            -- We have an exchange possibility
            -- New approach
            let matchOrder = entityVal match
            $(logInfo) $ pack $ show $ calculateOrderExecution order matchOrder
            redirect HomeR

            -- transfer transCur1 fullCur1Amt feeCur1 matchWallet userWallet matchOrder
            -- takeamt  transCur2 userWallet userOrder
            -- transfer transCur2 calcCur2Amt feeCur2 userWallet matchWallet userOrder

            let (Entity  matchId matchOrder) = match
            let availAmt = exchangeOrderAmountCents . entityVal $ match
                -- TODO:  FIXME:  Check several times truncation and rounding EVERYWHERE!
                targetInAmt = fromIntegral userAmountOutCents * userRatio
            -- 1 Record transaction reason for each wallet
            let matchUserId     = exchangeOrderUserId matchOrder
                matchCurrencyIn = exchangeOrderCurrencyIn matchOrder
                matchWalletInId = exchangeOrderWalletInId matchOrder
                matchRatio      = exchangeOrderRatio matchOrder
            (Entity _ userInWallet ) <-
                getOrCreateWalletDB userId userCurrencyIn
            (Entity _ matchInWallet) <-
                getOrCreateWalletDB matchUserId matchCurrencyIn
            userWalletTRId  <- insert $ WalletTransactionReason userWalletInId
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
            -- здесь userAmountIn потому что в ордере
            -- записано кол-во отдаваемой валюты,
            -- а не получаемой
            (matchOrderStatus, matchExecE) <- updateOrderData
                matchId
                userWalletTRId
                matchWalletTRId
                time
                matchOrderIsCompleted
                userAmountInCents
                matchFinalIncomeCents
                matchFeeCents
                (exchangeOrderStatus matchOrder)
            (userOrderStatus, userExecE) <- updateOrderData
                orderId
                matchWalletTRId
                userWalletTRId
                time
                userOrderIsCompleted
                matchAmountInCents
                userFinalIncomeCents
                userFeeCents
                (exchangeOrderStatus order)
            -- 4.2 User order
            -- здесь matchAmountIn потому что в ордере
            -- записано кол-во отдаваемой валюты,
            -- а не получаемой
            -- 4.3 Exchange profit
            mayProfit <- if targetRatio >= exchangeOrderRatio matchOrder
                then pure Nothing
                else do
                    -- We gain profit in user's OUT (A) currency when
                    -- Q A1 < Q A2
                    -- and in user's IN (B) currency when
                    -- Q A1 > Q A2
                    let profAmt = if userAmountOutCents < matchAmountInCents
                            then error "1 < 2"
                            else error "1 > 2"
                    let iprec = if userAmountOutCents < matchAmountInCents
                            then InnerProfitRecord userWalletTRId userCurrencyOut (error "amount")
                            else InnerProfitRecord matchWalletTRId userCurrencyIn (error "amount")
                    return $ Just $ error "profit"
            -- do it on paper now

            if userOrderIsCompleted
                then return (userOrderStatus, [ userExecE ])
                else do
                    (status, oes) <- orderExecutionStep userOrder
                    return (status, userExecE:oes)


calculateOrderExecution :: ExchangeOrder -> ExchangeOrder -> OrderExecData
calculateOrderExecution userOrder matchOrder = OrderExecData
    uFinalAmt
    (calcFeeCents defaultExchangeFee uFinalAmt)
    uFinalTrans
    mFinalAmt
    (calcFeeCents defaultExchangeFee mFinalAmt)
    mFinalTrans
    profC
    profit
    -- TODO: FIXME: TRUNCATE or ROUND?!
    where
        userRatio  = exchangeOrderRatio userOrder
        userOutAmt = exchangeOrderAmountCents userOrder
        userInAmt = truncate $ fromIntegral userOutAmt * userRatio
        -- Match
        matchRatio = exchangeOrderRatio matchOrder
        matchOutAmt = exchangeOrderAmountCents matchOrder
        matchInAmt = truncate $ fromIntegral matchOutAmt * matchRatio
        -- Currency
        userOutC = exchangeOrderCurrencyOut userOrder
        userInC = exchangeOrderCurrencyIn userOrder
        equalAmt = userOutAmt == matchInAmt
        equalRate = userRatio == 1 / matchRatio
        -- Profit
        profC
            | equalAmt = userOutC
            | userOutAmt < matchInAmt = userOutC
            | otherwise = userInC
        (uFinalAmt, uFinalTrans, mFinalAmt, mFinalTrans, profit) =
            if equalRate
                then let uamt = min userInAmt matchOutAmt
                         utrans = truncate $ fromIntegral uamt * userRatio
                         mtrans = truncate $ fromIntegral utrans * matchRatio
                     in ( uamt, utrans, utrans, mtrans, 0 )
                else
                    if userOutAmt < matchInAmt
                        then let uamt = truncate (fromIntegral userOutAmt * userRatio)
                                 mamt = truncate (fromIntegral uamt * matchRatio)
                            in ( uamt, userOutAmt, mamt, uamt, userOutAmt - mamt )
                        else let mamt = matchInAmt
                                 uamt = truncate (fromIntegral mamt * userRatio)
                            in ( uamt, mamt, mamt, matchOutAmt, matchOutAmt - uamt )


updateOrderData
    :: ExchangeOrderId
    -> WalletTransactionReasonId
    -> WalletTransactionReasonId
    -> UTCTime
    -> Bool
    -> Int
    -> Int
    -> Int
    -> ExchangeOrderStatus
    -> SqlPersistT Handler (ExchangeOrderStatus, Entity ExchangeOrderExecution)
updateOrderData orderId outReason inReason t isCompleted transfer income fee s = do
    let orderExec = ExchangeOrderExecution
            orderId
            t
            outReason
            inReason
            isCompleted
            transfer
            income
            fee
    orderExecId <- insert orderExec
    let status = if isCompleted
        then Executed t
        else PartiallyExecuted t $ transfer + case s of
            PartiallyExecuted _ x -> x
            _                     -> 0
    update
        orderId
        [ ExchangeOrderStatus   =. status
        , ExchangeOrderIsActive =.  not isCompleted
        , ExchangeOrderAmountCents -=. transfer
        ]
    return (status, Entity orderExecId orderExec)



data OrderExecData = OrderExecData
    { userOrderInAmountCents    :: Int
    , userOrderInFeeCents       :: Int
    , userOrderExecAmountCents  :: Int
    , matchOrderInAmountCents   :: Int
    , matchOrderInFeeCents      :: Int
    , matchOrderExecAmountCents :: Int
    , innerProfitCur            :: Currency
    , innerProfitAmountCents    :: Int
    }
    deriving Show

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Client.ExchangeOrderCreate where


import           Form.Exchanger.Order
import           Import
import           Local.Persist.Currency
import           Local.Persist.ExchangeOrder
import           Local.Persist.Wallet
import           Utils.Deposit               ( oneCoinCents )


{-

Пользователь
Отдаёт валюту А, X штук
Хочет валюту Б, Y штук
K1  = X/Y
K1' = Y/X

Существует
Отдаю валюту Б, W штук
Хочу валюту А, Z шук
K2  = Z/W
K2' = W/Z

Сделки не будет, если
K1  <  K2
K1' >  K2'

Поиск подходящего ордера: смотрим какое направление соотношения, это
определяет K или K' используем для сравнения.
k1  >= k2 или k1' <= k2'

При равных k1 == k2 (=> k1' == k2') можем выиграть на преобразовании

Рассмотрим сначала случай с равными соотношениями K1 == K2 и K1' == K2'
Если X == Z (тогда обязательно будет Y == W), то мы просто получаем 1%
с каждой стороны.

Если X > Z (что означает, что Y > W) то мы можем немного выиграть на
округлении. Если пользователь отдаёт больше, чем существующий хочет
получить, значит существующий ордер будет полностью закрыт, эти значения
должны быть точными, а сыграть на округлении мы сможем только при
переводе пользователю.  Переводиться будет точная сумма, а вот
начисляться будет путём преобразования:
y' = x' * k = x' / k' = z * k = z / k'
Получить разницу мы сможем, если y' < w

Аналогично, обратное
Z > X ==> W > Y
Пользователь отдаёт меньше, чем существущий хочет принять.
В этом случае пользовательский ордер будет закрыт полностью и поэтому
так нужно указать точные цифры, а существующий будет исполнен частично.
Отадавать сущ-ий будет точную цифру, то есть w' = x, а вот принимать
по формуле
z' = w' * k = w' / k' = x * k = x / k'
Разница наша, если z' < x


Теперь рассмотрим, что будет при
k1 > k2 (=> k1' < k2')

При обмене один ордер всегда будет ичсерпан (закрыт).

Возможны три ситуации:
x * k1' > w
x * k1' = w
x * k1' < w

1. Пользователь отдать готов больше, чем существуюший хочет получить
x > z


Мы сможем заработать, когда:
1. Пользователь отдаёт А дороже, чем существующий хочет за неё.
Заработаем на пользователе. Иными словами, за 1 А пользователь даёт
больше Б, чем существуюший хочет получить.  Иными словами, за 1 А
пользователь расчитывает получить меньше Б, чем существующий ордер
готов отдать. Получается, что можем заработать и на существующем ордере.

k1' > k2'
k2  > k1

Как же понять, на ком мы заработаем?  Так как при обмене всегда будет
закрывать один из ордеров, то:
1. закрывающийся ордер нужно выполнить точно, начислить ему
по установленному в оредере курсу монеты и списать существующую сумму.
А второму начислить ровно столько, сколько он ждёт за
переведённые деньги.  Как выяснить, какой ордер будет закрыт?

Пользователь отдёт Х А, существующий ждёт Z А.  Если
| x > z
| x > w * k2 > w / k2' -- заработаем на пользователе.


| y = x * k1 = x / k1'
| w = z * k2' = z / k2
| k1 < k2; k1' > k2'

y ?? w
x * k1 ?? w
x * k1 ?? z / k2
x * k1 * k2 ?? z
x > z, а это значит, что y > w,
если k1 * k2 >= 1.  Если k1 * k2 < 1, тогда оценить однозначно
нет возможности.

x < z
Пользователь может отдать А меньше, чем Существующий хочет получить.

В этих случаях нужно выяснить, кто хочет меньше. x < z || x > z
Отдаём всё тому, кто хочет меньше.  У него снимаем все средства
и закрываем ордер.

Второму переводим столько сколько он хотел.  Как есть.  Именно эту сумму
списываем у второго, но начисляем по расчёту на основе его соотношения.

x > z
То есть второй хочет получить меньше, чем даёт первый.
Закрываем второго.
Переводим ему Z - 1%
Его сумму W мы уже держим на руках.
Забираем у первого (списываем с ордера) x' = z
Разсчитываем сколько перевести:
y' = x' * k1' (= x' / k1)
По идее мы выигрываем на y, то есть на валюте Б.  Сравним y' и w

y' = x' * k1' = z * k1'
w = z * k2 = z / k2'

y' ?? w
z * k1' ?? z / k2'
k1' * k2' ??  1

****
k1 * k2' = 1, k1 == k2
k1 * (1/k2) = 1, k1 == k2
k1 < k2
k1 * (1/k2)


Как определить кто хочет больше?  Просто сравнить x и z.



2.


x = y * k1'
x = y / k
z = w * k2
x = w / k2'



k1' < k2'



Ход мыслей

Пользователь надеется получить Б
за одну Б отдаёт K1' А
за одну А хочет  K1  Б

Проверкка:
x = 10  (a = pzm)
y = 100 (b = rur)
k1  = 0.1 (1/10)  k pzm -> 1 rur
k1' = 10          k rur -> 1 pzm
-----
Верно
-----

Сделки не будет, если существующий ордер готов за одну А
предлагать меньше Б, чем расчитывает пользователь.  В виде формулы:
k2' < k1'
Так же сделки не будет, если существуюший ордер за 1 Б хочет больше,
чем пользователь готов пледложить, то есть
k2  > k1



-}


postExchangeOrderCreateR :: Handler Html
postExchangeOrderCreateR = do
    clientId <- requireClientId
    ((res, widget), enctype) <- runFormPost $ createOrderForm ExchangePzmRur
    $(logInfo) $ pack . show $ res
    case res of
        FormFailure es -> defaultLayout $ do
            setMessage $ renderFormErrors es
            redirect HomeR
        FormMissing -> redirect HomeR
        FormSuccess orderData -> do
            let oact = action orderData
                oamt = amount orderData
                opair = pair orderData
                oratio = ratio orderData
            -- TODO write a function for this: case (action, pair) of ...
            -- 1. Check if required coins amount is available in user's wallet
            let (currency, inCurrency) = case opair of
                    ExchangePzmRur -> if oact == EAGive then (pzmC, rurC) else (rurC, pzmC)
                    ExchangeRurPzm -> if oact == EAGive then (rurC, pzmC) else (pzmC, rurC)
            wout <- getOrCreateWallet clientId currency
            win  <- getOrCreateWallet clientId inCurrency
            now <- liftIO getCurrentTime
            let excDirRatio = fromNormalizedRatio opair oratio
                mamt = multAmt excDirRatio oamt
            let (outAmt, inAmt) = if oact == EAGive
                    then (oamt, mamt)
                    else (mamt, oamt)
                wamt = userWalletAmountCents . entityVal $ wout
            let fee = calcFeeCents defaultExchangeFee inAmt
            if wamt < outAmt || outAmt <= 0
                then do
                    let errorMsg = if outAmt <= 0
                            then "Неверная сумма" :: Text
                            else "Недостаточно средств"
                    setMessage [shamlet|<p>#{errorMsg}|]
                    redirect HomeR
                else do
                    -- Freeze user coins
                    let exchange = if oact == EAGive
                            then opair
                            else flipPair opair
                    (trid, _) <- freezeCoins clientId wout outAmt now
                    -- Save order
                    -- TODO: FIXME: Save order transactionally with
                    -- instant exchnage (if possible).  We don't want
                    -- to have unexecuted orders.  In other words --
                    -- there shouldn't be matching active orders
                    -- in database at any moment.  If order have match
                    -- it must be instantly executed
                    (orderId, savedOrder) <- saveOrder clientId exchange oratio fee outAmt trid
                    -- Check matching orders
                    let ratioCondition =
                            if opair == defPairDir opair
                            then [ ExchangeOrderNormalizedRatio <=. oratio ]
                            else [ ExchangeOrderNormalizedRatio >=. oratio ]
                    let orderRatioN = exchangeOrderRatioNoramlization savedOrder
                    morders <- runDB $ selectList
                            (   ratioCondition <>
                              [ ExchangeOrderIsActive ==. True
                              , ExchangeOrderAmountLeft >=. 0
                              , ExchangeOrderId !=. orderId
                              , ExchangeOrderRatioNoramlization ==. orderRatioN
                              , ExchangeOrderPair ==. flipPair exchange ] )
                            [ Asc ExchangeOrderCreated ]
                    $(logInfo) $ "Matching orders: " <> (pack . show $ morders)
                    if length morders > 0
                        then do
                            -- take first order
                            let ( ( Entity matchId mo ) : mrest ) = morders
                            let mratio  = exchangeOrderNormalizedRatio mo
                                mnormd  = exchangeOrderRatioNoramlization mo
                                mpair   = exchangeOrderPair mo
                                mOutAmt = exchangeOrderAmountLeft mo
                                uOutAmt   = exchangeOrderAmountLeft savedOrder
                            let uRatio  = exchangeOrderNormalizedRatio savedOrder
                                uNormD  = exchangeOrderRatioNoramlization savedOrder
                                uPair   = exchangeOrderPair savedOrder
                                uDirRatio = normalizeRatio uNormD uPair uRatio
                            let mDirRatio = normalizeRatio mnormd mpair mratio
                            -- let's calculate what order will be
                            -- fully executed
                            -- x = outAmt cur.A is currency
                            -- z = calculate pair amount ratio
                            let uInAmt = multAmt uDirRatio uOutAmt
                            let mInAmt = multAmt mDirRatio mOutAmt
                            $(logInfo) $ "M | Ratio direct: " <> (pack . show) mDirRatio <> "; amount = " <> (pack . show) mInAmt <> " | Out: " <> (pack . show) mOutAmt
                            $(logInfo) $ "U | In: " <> (pack . show) mInAmt <> "; Out: " <> (pack . show) mOutAmt
                            let mUserId = exchangeOrderUserId mo
                            uWIn  <- getOrCreateWallet clientId inCurrency
                            uWOut <- getOrCreateWallet clientId   currency
                            mWIn  <- getOrCreateWallet mUserId    currency
                            mWOut <- getOrCreateWallet mUserId  inCurrency
                            let setFullyExecuted =
                                    [ ExchangeOrderStatus =. Executed now
                                    , ExchangeOrderAmountLeft =. 0
                                    , ExchangeOrderIsActive =. False ]
                            let uInAmtB = userWalletAmountCents (entityVal uWIn)
                                mInAmtB = userWalletAmountCents (entityVal mWIn)
                            let userWInId   = entityKey uWIn
                                userWOutId  = entityKey uWOut
                                matchWInId  = entityKey mWIn
                                matchWOutId = entityKey mWOut
                            if uOutAmt == mInAmt
                                -- Amount is equal.
                                then do
                                    let ( finalUInFee, finalMInFee, finalUInAmt, finalMInAmt, diffProfit ) =
                                            if oratio == mratio
                                            then ( calcFeeCents defaultExchangeFee mOutAmt
                                                 , calcFeeCents defaultExchangeFee uOutAmt
                                                 , mOutAmt - finalUInFee
                                                 , uOutAmt - finalMInFee
                                                 , 0 )
                                            else
                                                -- In this case user pays more.
                                                -- Therefore, user expects
                                                -- less than match gives.
                                                -- Match will be fully executed
                                                -- So, we take from match B fully.
                                                ( calcFeeCents defaultExchangeFee uInAmt
                                                 , calcFeeCents defaultExchangeFee mInAmt
                                                 , uInAmt - finalUInFee
                                                 , mInAmt - finalMInFee
                                                 , mOutAmt - uInAmt )
                                    -- Exchange orders having equal ratio and in/out amount
                                    $(logInfo) $ "USER >>>" <> (pack . show) uOutAmt <> " <<< " <> (pack . show) finalUInAmt <> " - " <> (pack . show) finalUInFee
                                    $(logInfo) $ "MATCH >>>" <> (pack . show) mOutAmt <> " <<< " <> (pack . show) finalMInAmt <> " - " <> (pack . show) finalMInFee
                                    -- Exchange orders having equal in/out amount
                                    -- Both orders will be closed
                                    $(logInfo) $ "USER >>>" <> (pack . show) uOutAmt <> " <<< " <> (pack . show) finalUInAmt <> " - " <> (pack . show) finalUInFee
                                    $(logInfo) $ "MATCH >>>" <> (pack . show) mOutAmt <> " <<< " <> (pack . show) finalMInAmt <> " - " <> (pack . show) finalMInFee
                                    runDB $ do
                                        -- Update order statuses and data
                                        update matchId setFullyExecuted
                                        update orderId setFullyExecuted
                                        -- Update users balances
                                        update userWInId [ UserWalletAmountCents +=. finalUInAmt ]
                                        update matchWInId [ UserWalletAmountCents +=. finalMInAmt ]
                                        -- Record transaction details
                                        -- First create transaction reasons
                                        uRId <- insert $ WalletTransactionReason userWInId
                                        mRId <- insert $ WalletTransactionReason matchWInId
                                        -- Make chronological logs
                                        -- Wallet balances
                                        insert $ WalletBalanceTransaction userWInId (ExchangeExchange finalUInAmt) uRId uInAmtB now
                                        insert $ WalletBalanceTransaction matchWInId (ExchangeExchange finalMInAmt) mRId mInAmtB now
                                        -- Order execution
                                        insert $ ExchangeOrderExecution orderId now mRId uRId True uOutAmt mOutAmt finalUInFee
                                        insert $ ExchangeOrderExecution matchId now uRId mRId True mOutAmt uOutAmt finalMInFee
                                        -- Save fee data
                                        insert $ InnerProfitRecord uRId inCurrency finalUInFee ExchangeFee
                                        insert $ InnerProfitRecord mRId currency finalMInFee ExchangeFee
                                        when (diffProfit > 0) $ do
                                            $(logInfo) $ "PROFIT +++ " <> (pack . show) diffProfit <> currSign inCurrency
                                            insert $ InnerProfitRecord mRId currency diffProfit ExchangeDiff
                                            return ()
                                    defaultLayout $ do
                                        setMessage "Ордер исполнен моментально"
                                        redirect HomeR
                                else do
                                    let ( userFinalOut, userIn, userFinalIn, userFinalFee,
                                          matchFinalOut, matchIn, matchFinalIn, matchFinalFee,
                                          diffProfit, diffCurrency, userOrderUpdates, matchOrderUpdates ) =
                                            if outAmt > mInAmt
                                                then let uout = mInAmt
                                                         uin  = multAmt uDirRatio uout
                                                         ufee = calcFeeCents defaultExchangeFee uin
                                                         mout = mOutAmt
                                                         min  = mInAmt
                                                         mfee = calcFeeCents defaultExchangeFee min
                                                         st   = exchangeOrderStatus savedOrder
                                                    in ( uout, uin, uin - ufee, ufee,
                                                         mout, min, min - mfee, mfee,
                                                         mout - uin, inCurrency, setPartiallyExecuted uout now st, setFullyExecuted )
                                                else let uout = uOutAmt
                                                         uin  = uInAmt
                                                         ufee = calcFeeCents defaultExchangeFee uin
                                                         min  = uout
                                                         mfee = calcFeeCents defaultExchangeFee min
                                                         mout = multAmt (1 / mDirRatio) min
                                                         st = exchangeOrderStatus mo
                                                    in ( uout, uin, uin - ufee, ufee,
                                                         mout, min, min - mfee, mfee,
                                                         mout - uin, inCurrency, setFullyExecuted, setPartiallyExecuted mout now st )
                                    $(logInfo) $ "U out: " <> (pack . show) userFinalOut <> "; in: " <> (pack . show) userFinalIn <> " ; fee: " <> (pack . show) userFinalFee
                                    $(logInfo) $ "M out: " <> (pack . show) matchFinalOut <> " ; in: " <> (pack . show) matchFinalIn <> " ; fee: " <> (pack . show) matchFinalFee
                                    $(logInfo) $ "Diff: " <> (pack . show) diffProfit
                                    runDB $ do
                                        -- Update order statuses and data
                                        update orderId userOrderUpdates
                                        update matchId matchOrderUpdates
                                        -- Update users balances
                                        update userWInId [ UserWalletAmountCents +=. userFinalIn ]
                                        update matchWInId [ UserWalletAmountCents +=. matchFinalIn ]
                                        -- Record transaction details
                                        -- First create transaction reasons
                                        uRId <- insert $ WalletTransactionReason userWInId
                                        mRId <- insert $ WalletTransactionReason matchWInId
                                        -- Make chronological logs
                                        -- Wallet balances
                                        insert $ WalletBalanceTransaction userWInId (ExchangeExchange userFinalIn) uRId uInAmtB now
                                        insert $ WalletBalanceTransaction matchWInId (ExchangeExchange matchFinalIn) mRId mInAmtB now
                                        -- Order execution
                                        insert $ ExchangeOrderExecution orderId now mRId uRId (uOutAmt == userFinalOut) userFinalOut matchFinalOut userFinalFee
                                        insert $ ExchangeOrderExecution matchId now uRId mRId (mOutAmt == matchFinalOut) matchFinalOut userFinalOut matchFinalFee
                                        -- Save fee data
                                        insert $ InnerProfitRecord uRId inCurrency userFinalFee ExchangeFee
                                        insert $ InnerProfitRecord mRId currency matchFinalFee ExchangeFee
                                        when (diffProfit > 0) $ do
                                            insert $ InnerProfitRecord mRId diffCurrency diffProfit ExchangeDiff
                                            return ()
                                    if outAmt <= mInAmt
                                    then defaultLayout $ do
                                        setMessage "Ордер исполнен моментально"
                                        redirect HomeR
                                    else error "recursive order execution"
                        else do
                            setMessage "Ордер на обмен создан"
                            redirect HomeR
    redirect HomeR
  where
    multAmt = convertCents
    freezeCoins uid ewallet t = runDB . freezeUserCoins uid ewallet t
    saveOrder uid pair ratio fee amount trid = runDB $ do
        time <- liftIO getCurrentTime
        let ratioNorm = defPairDir pair
            status = Created time
        let order = ExchangeOrder
                uid pair amount amount ratioNorm ratio fee time status True trid
        oid <- insert order
        return (oid, order)
    setPartiallyExecuted :: Int -> UTCTime -> ExchangeOrderStatus -> [ Update ExchangeOrder ]
    setPartiallyExecuted x t s =
        [ ExchangeOrderStatus =. PartiallyExecuted t ((x +) $ case s of
            PartiallyExecuted _ p -> p
            _                     -> 0)
        , ExchangeOrderAmountLeft -=. x ]
    renderFormErrors _ = error "wip" -- setMessage * redirect HomeR

freezeUserCoins
    :: UserId
    -> Entity UserWallet
    -> Int
    -> UTCTime
    -> SqlPersistT Handler
        ( WalletTransactionReasonId, WalletBalanceTransactionId )
freezeUserCoins userId eWallet amount t = do
    let (Entity walletId wallet) = eWallet
    let tr = WalletTransactionReason walletId
        famt = userWalletAmountCents wallet
    -- save transaction reason
    trid' <- insert tr
    -- decrease balance
    update walletId [ UserWalletAmountCents -=. amount ]
    -- save balance transaction record
    wtid' <- insert $ WalletBalanceTransaction
        walletId (ExchangeFreeze $ negate amount) trid' famt t
    return (trid', wtid')


-- exchangeStep :: Entity ExchangeOrder -> [ Entity ExchangeOrder ] -> SqlPersistT Handler ()
-- exchangeStep userOrderE matches logs = do
--     error "Wip"


-- postExchangeOrderCreateR :: Handler Html
-- postExchangeOrderCreateR = do
--     clientData <- requireClient
--     ((res, widget), enctype) <- runFormPost formCreateExchageOrder
--     case res of
--         FormMissing -> invalidArgs ["Данные не получены"]
--         FormFailure es -> do
--             setMessage [shamlet|
--                 <p .h5>Ошибка создания ордера на обмен
--                 <ul>
--                     $forall e <- es
--                         <li>#{e}
--                 |]
--             redirect HomeR
--         FormSuccess ExchangeOrderData{..} -> do
--             let (Entity userId _, wallets) = clientData
--             outWallet <- getMatchingWallet userId orderDataCurrencyOut wallets
--             inWallet  <- getMatchingWallet userId orderDataCurrencyIn wallets
--             if (userWalletAmountCents $ entityVal outWallet) < orderDataAmountCents
--                 then defaultLayout $ do
--                     setTitle [shamlet|
--                         <p>Для создания ордера недостаточно средств на счёте
--                         <p>Пополните баланс|]
--                     redirect HomeR
--                 else do
--                     time <- liftIO getCurrentTime
--                     _ <- runDB $ do
--                         let outWalletId = entityKey outWallet
--                             inWalletId = entityKey inWallet
--                         wtrId <- insert $ WalletTransactionReason outWalletId
--                         wbtId <- insert $ WalletBalanceTransaction
--                             outWalletId
--                             (ExchangeFreeze orderDataAmountCents)
--                             wtrId
--                             (userWalletAmountCents . entityVal $ outWallet)
--                         update outWalletId [UserWalletAmountCents -=. orderDataAmountCents]
--                         let order = ExchangeOrder
--                                 userId
--                                 inWalletId
--                                 outWalletId
--                                 orderDataCurrencyOut
--                                 orderDataCurrencyIn
--                                 orderDataAmountCents
--                                 orderDataRatio
--                                 orderDataExpectedFeeCents
--                                 time
--                                 (Created time)
--                                 True
--                                 wtrId
--                         orderId <- insert order
--                         -- TODO: FIXME: Check if order can be executed already
--                         instantOrderExecution (Entity orderId order)
--                     defaultLayout $ do
--                         setMessage [shamlet|<p .h5>Ордер успешно создан|]
--                         redirect HomeR



-- getMatchingWallet :: UserId -> Currency -> [Entity UserWallet] -> Handler (Entity UserWallet)
-- getMatchingWallet userId currency wallets = do
--     let mwallet = find (\(Entity _ UserWallet{..}) -> userWalletCurrency == currency) wallets
--     case mwallet of
--         Nothing     -> getOrCreateWallet userId currency
--         Just wallet -> return wallet

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



-- instantOrderExecution :: Entity ExchangeOrder -> SqlPersistT Handler (ExchangeOrderStatus, [Entity ExchangeOrderExecution])
-- instantOrderExecution = orderExecutionStep

-- orderExecutionStep
--     :: Entity ExchangeOrder
--     -> SqlPersistT Handler (ExchangeOrderStatus, [Entity ExchangeOrderExecution])
--     -- ^ Bool indicates is order fully executed
-- orderExecutionStep userOrder = do
--     let (Entity orderId order) = userOrder
--     let userId = exchangeOrderUserId order
--         userCurrencyIn = exchangeOrderCurrencyIn order
--         userCurrencyOut = exchangeOrderCurrencyOut order
--         userWalletInId = exchangeOrderWalletInId order
--         userAmountOutCents = exchangeOrderAmountCents order
--         userRatio  = exchangeOrderRatio order
--         targetRatio = 1 / userRatio
--     -- TODO: FIXME: Prevent archieved and cancelled orders to be selected!!
--     mayOrder <- selectFirst
--         [ ExchangeOrderIsActive ==. True
--         , ExchangeOrderCurrencyOut ==. userCurrencyIn
--         , ExchangeOrderRatio <=. targetRatio ]
--         [ Asc ExchangeOrderRatio
--         , Asc ExchangeOrderCreated ]
--     case mayOrder of
--         Nothing -> return (exchangeOrderStatus order, [])
--         Just match -> do
--             -- We have an exchange possibility
--             -- New approach
--             let matchOrder = entityVal match
--             $(logInfo) $ pack $ show $ calculateOrderExecution order matchOrder
--             redirect HomeR

--             -- transfer transCur1 fullCur1Amt feeCur1 matchWallet userWallet matchOrder
--             -- takeamt  transCur2 userWallet userOrder
--             -- transfer transCur2 calcCur2Amt feeCur2 userWallet matchWallet userOrder

--             let (Entity  matchId matchOrder) = match
--             let availAmt = exchangeOrderAmountCents . entityVal $ match
--                 -- TODO:  FIXME:  Check several times truncation and rounding EVERYWHERE!
--                 targetInAmt = fromIntegral userAmountOutCents * userRatio
--             -- 1 Record transaction reason for each wallet
--             let matchUserId     = exchangeOrderUserId matchOrder
--                 matchCurrencyIn = exchangeOrderCurrencyIn matchOrder
--                 matchWalletInId = exchangeOrderWalletInId matchOrder
--                 matchRatio      = exchangeOrderRatio matchOrder
--             (Entity _ userInWallet ) <-
--                 getOrCreateWalletDB userId userCurrencyIn
--             (Entity _ matchInWallet) <-
--                 getOrCreateWalletDB matchUserId matchCurrencyIn
--             userWalletTRId  <- insert $ WalletTransactionReason userWalletInId
--             matchWalletTRId <- insert $ WalletTransactionReason matchWalletInId
--             -- 2 update wallet balances
--             -- TODO: FIXME: TRUNCATE?
--             let userAmountInCents = min (truncate targetInAmt) availAmt
--                 userFeeCents = calcFeeCents defaultExchangeFee userAmountInCents
--                 userFinalIncomeCents = userAmountInCents - userFeeCents
--             let matchAmountInCents = truncate $ fromIntegral userAmountInCents * matchRatio
--                 matchFeeCents = calcFeeCents defaultExchangeFee matchAmountInCents
--                 matchFinalIncomeCents = matchAmountInCents - matchFeeCents
--             -- 2.1 user balance income
--             userWalletBTId <- insert $ WalletBalanceTransaction
--                 userWalletInId
--                 (ExchangeExchange userFinalIncomeCents)
--                 userWalletTRId
--                 (userWalletAmountCents userInWallet)
--             update userWalletInId [UserWalletAmountCents +=. userFinalIncomeCents]
--             -- 2.2 match balance id
--             matchWalletBTId <- insert $ WalletBalanceTransaction
--                 matchWalletInId
--                 (ExchangeExchange matchFinalIncomeCents)
--                 matchWalletTRId
--                 (userWalletAmountCents matchInWallet)
--             update matchWalletInId [UserWalletAmountCents +=. matchFinalIncomeCents]
--             -- 3. Record fees
--             time <- liftIO getCurrentTime
--             userFeeInId <- insert $ FeeIncome userCurrencyIn userFeeCents userWalletTRId time
--             matchFeeInId <- insert $ FeeIncome matchCurrencyIn matchFeeCents matchWalletTRId time
--             -- 4. Record execution order statuses and events
--             let userOrderIsCompleted = fromIntegral availAmt >= targetInAmt
--                 matchOrderIsCompleted = fromIntegral availAmt <= targetInAmt
--             -- 4.1 Before recursively execute user order it's better
--             -- to record match order execution and update
--             -- its status
--             -- здесь userAmountIn потому что в ордере
--             -- записано кол-во отдаваемой валюты,
--             -- а не получаемой
--             (matchOrderStatus, matchExecE) <- updateOrderData
--                 matchId
--                 userWalletTRId
--                 matchWalletTRId
--                 time
--                 matchOrderIsCompleted
--                 userAmountInCents
--                 matchFinalIncomeCents
--                 matchFeeCents
--                 (exchangeOrderStatus matchOrder)
--             (userOrderStatus, userExecE) <- updateOrderData
--                 orderId
--                 matchWalletTRId
--                 userWalletTRId
--                 time
--                 userOrderIsCompleted
--                 matchAmountInCents
--                 userFinalIncomeCents
--                 userFeeCents
--                 (exchangeOrderStatus order)
--             -- 4.2 User order
--             -- здесь matchAmountIn потому что в ордере
--             -- записано кол-во отдаваемой валюты,
--             -- а не получаемой
--             -- 4.3 Exchange profit
--             mayProfit <- if targetRatio >= exchangeOrderRatio matchOrder
--                 then pure Nothing
--                 else do
--                     -- We gain profit in user's OUT (A) currency when
--                     -- Q A1 < Q A2
--                     -- and in user's IN (B) currency when
--                     -- Q A1 > Q A2
--                     let profAmt = if userAmountOutCents < matchAmountInCents
--                             then error "1 < 2"
--                             else error "1 > 2"
--                     let iprec = if userAmountOutCents < matchAmountInCents
--                             then InnerProfitRecord userWalletTRId userCurrencyOut (error "amount")
--                             else InnerProfitRecord matchWalletTRId userCurrencyIn (error "amount")
--                     return $ Just $ error "profit"
--             -- do it on paper now

--             if userOrderIsCompleted
--                 then return (userOrderStatus, [ userExecE ])
--                 else do
--                     (status, oes) <- orderExecutionStep userOrder
--                     return (status, userExecE:oes)


-- calculateOrderExecution :: ExchangeOrder -> ExchangeOrder -> OrderExecData
-- calculateOrderExecution userOrder matchOrder = OrderExecData
--     uFinalAmt
--     (calcFeeCents defaultExchangeFee uFinalAmt)
--     uFinalTrans
--     mFinalAmt
--     (calcFeeCents defaultExchangeFee mFinalAmt)
--     mFinalTrans
--     profC
--     profit
--     -- TODO: FIXME: TRUNCATE or ROUND?!
--     where
--         userRatio  = exchangeOrderRatio userOrder
--         userOutAmt = exchangeOrderAmountCents userOrder
--         userInAmt = truncate $ fromIntegral userOutAmt * userRatio
--         -- Match
--         matchRatio = exchangeOrderRatio matchOrder
--         matchOutAmt = exchangeOrderAmountCents matchOrder
--         matchInAmt = truncate $ fromIntegral matchOutAmt * matchRatio
--         -- Currency
--         userOutC = exchangeOrderCurrencyOut userOrder
--         userInC = exchangeOrderCurrencyIn userOrder
--         equalAmt = userOutAmt == matchInAmt
--         equalRate = userRatio == 1 / matchRatio
--         -- Profit
--         profC
--             | equalAmt = userOutC
--             | userOutAmt < matchInAmt = userOutC
--             | otherwise = userInC
--         (uFinalAmt, uFinalTrans, mFinalAmt, mFinalTrans, profit) =
--             if equalRate
--                 then let uamt = min userInAmt matchOutAmt
--                          utrans = truncate $ fromIntegral uamt * userRatio
--                          mtrans = truncate $ fromIntegral utrans * matchRatio
--                      in ( uamt, utrans, utrans, mtrans, 0 )
--                 else
--                     if userOutAmt < matchInAmt
--                         then let uamt = truncate (fromIntegral userOutAmt * userRatio)
--                                  mamt = truncate (fromIntegral uamt * matchRatio)
--                             in ( uamt, userOutAmt, mamt, uamt, userOutAmt - mamt )
--                         else let mamt = matchInAmt
--                                  uamt = truncate (fromIntegral mamt * userRatio)
--                             in ( uamt, mamt, mamt, matchOutAmt, matchOutAmt - uamt )


-- updateOrderData
--     :: ExchangeOrderId
--     -> WalletTransactionReasonId
--     -> WalletTransactionReasonId
--     -> UTCTime
--     -> Bool
--     -> Int
--     -> Int
--     -> Int
--     -> ExchangeOrderStatus
--     -> SqlPersistT Handler (ExchangeOrderStatus, Entity ExchangeOrderExecution)
-- updateOrderData orderId outReason inReason t isCompleted transfer income fee s = do
--     let orderExec = ExchangeOrderExecution
--             orderId
--             t
--             outReason
--             inReason
--             isCompleted
--             transfer
--             income
--             fee
--     orderExecId <- insert orderExec
--     let status = if isCompleted
--         then Executed t
--         else PartiallyExecuted t $ transfer + case s of
--             PartiallyExecuted _ x -> x
--             _                     -> 0
--     update
--         orderId
--         [ ExchangeOrderStatus   =. status
--         , ExchangeOrderIsActive =.  not isCompleted
--         , ExchangeOrderAmountCents -=. transfer
--         ]
--     return (status, Entity orderExecId orderExec)



-- data OrderExecData = OrderExecData
--     { userOrderInAmountCents    :: Int
--     , userOrderInFeeCents       :: Int
--     , userOrderExecAmountCents  :: Int
--     , matchOrderInAmountCents   :: Int
--     , matchOrderInFeeCents      :: Int
--     , matchOrderExecAmountCents :: Int
--     , innerProfitCur            :: Currency
--     , innerProfitAmountCents    :: Int
--     }
--     deriving Show

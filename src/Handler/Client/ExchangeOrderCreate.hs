{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Client.ExchangeOrderCreate where


import           Form.Exchanger.Order
import           Import
import           Local.Persist.Currency
import           Local.Persist.ExchangeOrder
import           Local.Persist.Wallet

import           Data.Maybe                  ( fromJust )


postExchangeOrderCreateR :: Handler Html
postExchangeOrderCreateR = do
    clientId <- requireClientId
    ((res, _), _) <- runFormPost $ createOrderForm ExchangePzmRur
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
            -- TODO: write a function for this: case (action, pair) of ...
            -- 1. Check if required coins amount is available in user's wallet
            -- TODO: FIXME: use flipPair and unPairCurrency
            let (currency, _) = case opair of
                    ExchangePzmRur -> if oact == EAGive then (pzmC, rurC) else (rurC, pzmC)
                    ExchangeRurPzm -> if oact == EAGive then (rurC, pzmC) else (pzmC, rurC)
            wout <- getOrCreateWallet clientId currency
            -- win  <- getOrCreateWallet clientId inCurrency
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
                    setMessage [shamlet|<div .error>#{errorMsg}|]
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
                    let orderRatioN = exchangeOrderRatioNormalization savedOrder
                    morders <- runDB $ selectList
                            (   ratioCondition <>
                              [ ExchangeOrderIsActive ==. True
                              , ExchangeOrderAmountLeft >=. 0
                              , ExchangeOrderId !=. orderId
                              , ExchangeOrderRatioNormalization ==. orderRatioN
                              , ExchangeOrderPair ==. flipPair exchange ] )
                            [ Asc ExchangeOrderCreated ]
                    $(logInfo) $ "Matching orders: " <> (pack . show $ morders)
                    exchRes <- exchangeOrders (Entity orderId savedOrder) morders []
                    setMessage $ case exchRes of
                        [] -> "Ордер создан"
                        (tOrderClosed, _, _, _, _, _, _, _) : _ ->
                            if tOrderClosed
                            then "Ордер создан и полностью исполнен"
                            else "Ордер создан и частично исполнен"
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
    renderFormErrors es = [shamlet|
        $forall error <- es
            <div .error>#{error}
        |]

    exchangeOrders _      []      acc = return acc
    exchangeOrders target matches acc = do
        -- take first order
        let ( Entity tOrderId tOrder )             = target
            ( Entity mOrderId mOrder : mRest ) = matches
        let tOutAmtLeft = exchangeOrderAmountLeft tOrder
            tRatio      = exchangeOrderNormalizedRatio tOrder
            tNormD      = exchangeOrderRatioNormalization tOrder
            tPair       = exchangeOrderPair tOrder
            tDirRatio   = normalizeRatio tNormD tPair tRatio
        let mOutAmtLeft = exchangeOrderAmountLeft mOrder
            mRatio      = exchangeOrderNormalizedRatio mOrder
            mNormD      = exchangeOrderRatioNormalization mOrder
            mPair       = exchangeOrderPair mOrder
            mDirRatio   = normalizeRatio mNormD mPair mRatio
        let tUserId = exchangeOrderUserId tOrder
            mUserId = exchangeOrderUserId mOrder
        let ( currencyA, currencyB ) = unPairCurrency tPair
        -- let's calculate what order will be fully executed
        let tInAmtExpects = multAmt tDirRatio tOutAmtLeft
        let mInAmtExpects = multAmt mDirRatio mOutAmtLeft
        $(logInfo) $ "M | Ratio direct: " <> (pack . show) mDirRatio <> "; amount = " <> (pack . show) mInAmtExpects <> " | Out: " <> (pack . show) mOutAmtLeft
        $(logInfo) $ "U | In: " <> (pack . show) mInAmtExpects <> "; Out: " <> (pack . show) mOutAmtLeft
        -- tWalletOut <- getOrCreateWallet tUserId currencyA
        tWalletIn@(Entity tWalletInId _)  <- getOrCreateWallet tUserId currencyB
        -- mWalletOut <- getOrCreateWallet mUserId currencyB
        mWalletIn@(Entity mWalletInId _)  <- getOrCreateWallet mUserId currencyA
        timeNow <- liftIO getCurrentTime
        let setFullyExecuted =
                [ ExchangeOrderStatus =. Executed timeNow
                , ExchangeOrderAmountLeft =. 0
                , ExchangeOrderIsActive =. False ]
        let tWalletInBalance = userWalletAmountCents (entityVal tWalletIn)
            mWalletInBalance = userWalletAmountCents (entityVal mWalletIn)
        let userWInId   = entityKey tWalletIn
            -- userWOutId  = entityKey tWalletOut
            matchWInId  = entityKey mWalletIn
            -- matchWOutId = entityKey mWalletOut
        if tOutAmtLeft == mInAmtExpects
            -- Amount is equal.
            then do
                let ( finalUInFee, finalMInFee, finalUInAmt, finalMInAmt, diffProfit ) =
                        if tRatio == mRatio
                        then ( calcFeeCents defaultExchangeFee mOutAmtLeft
                             , calcFeeCents defaultExchangeFee tOutAmtLeft
                             , mOutAmtLeft - finalUInFee
                             , tOutAmtLeft - finalMInFee
                             , 0 )
                        else
                            -- In this case user pays more.
                            -- Therefore, user expects
                            -- less than match gives.
                            -- Match will be fully executed
                            -- So, we take from match B fully.
                            ( calcFeeCents defaultExchangeFee tInAmtExpects
                            , calcFeeCents defaultExchangeFee mInAmtExpects
                            , tInAmtExpects - finalUInFee
                            , mInAmtExpects - finalMInFee
                            , mOutAmtLeft - tInAmtExpects )
                -- Exchange orders having equal ratio and in/out amount
                $(logInfo) $ "USER >>>" <> (pack . show) tOutAmtLeft <> " <<< " <> (pack . show) finalUInAmt <> " - " <> (pack . show) finalUInFee
                $(logInfo) $ "MATCH >>>" <> (pack . show) mOutAmtLeft <> " <<< " <> (pack . show) finalMInAmt <> " - " <> (pack . show) finalMInFee
                -- Exchange orders having equal in/out amount
                -- Both orders will be closed
                $(logInfo) $ "USER >>>" <> (pack . show) tOutAmtLeft <> " <<< " <> (pack . show) finalUInAmt <> " - " <> (pack . show) finalUInFee
                $(logInfo) $ "MATCH >>>" <> (pack . show) mOutAmtLeft <> " <<< " <> (pack . show) finalMInAmt <> " - " <> (pack . show) finalMInFee
                runDB $ do
                    -- Update order statuses and data
                    update mOrderId setFullyExecuted
                    update tOrderId setFullyExecuted
                    -- Update users balances
                    update userWInId  [ UserWalletAmountCents +=. finalUInAmt ]
                    update matchWInId [ UserWalletAmountCents +=. finalMInAmt ]
                    -- Record transaction details
                    -- First create transaction reasons
                    tRId <- insert $ WalletTransactionReason userWInId
                    mRId <- insert $ WalletTransactionReason matchWInId
                    -- Make chronological logs
                    -- Wallet balances
                    tTransRec <- insert $ WalletBalanceTransaction userWInId (ExchangeExchange finalUInAmt) tRId  tWalletInBalance timeNow
                    mTransRec <- insert $ WalletBalanceTransaction matchWInId (ExchangeExchange finalMInAmt) mRId mWalletInBalance timeNow
                    -- Order execution
                    tExecRec <- insert $ ExchangeOrderExecution tOrderId timeNow mRId tRId True tOutAmtLeft mOutAmtLeft finalUInFee
                    mExecRec <- insert $ ExchangeOrderExecution mOrderId timeNow tRId mRId True mOutAmtLeft tOutAmtLeft finalMInFee
                    -- Save fee data
                    tFeeProfitRec <- insert $ InnerProfitRecord tRId currencyB finalUInFee ExchangeFee
                    mFeeProfitRec <- insert $ InnerProfitRecord mRId currencyA finalMInFee ExchangeFee
                    $(logInfo) $ "PROFIT +++ " <> (pack . show) diffProfit <> " " <> currSign currencyB
                    diffProfitRec <- if diffProfit > 0
                        then do
                            diffProfitId <- insert $ InnerProfitRecord mRId currencyA diffProfit ExchangeDiff
                            return [ diffProfitId ]
                        else return []
                    let profitRec = tFeeProfitRec : mFeeProfitRec : diffProfitRec
                    return $ (True, tRId, mRId, tTransRec, mTransRec, tExecRec, mExecRec, profitRec) : acc
                -- defaultLayout $ do
                --     setMessage "Ордер исполнен моментально"
                --     redirect HomeR
            else do
                let ( userFinalOut, userIn, userFinalIn, userFinalFee,
                      matchFinalOut, matchIn, matchFinalIn, matchFinalFee,
                      diffProfit, diffCurrency, userOrderUpdates, matchOrderUpdates ) =
                        if tOutAmtLeft > mInAmtExpects
                            then let tout = mInAmtExpects
                                     tin  = convertCents tDirRatio tout
                                     tfee = calcFeeCents defaultExchangeFee tin
                                     mout = mOutAmtLeft
                                     min  = mInAmtExpects
                                     mfee = calcFeeCents defaultExchangeFee min
                                     st   = exchangeOrderStatus tOrder
                                in ( tout, tin, tin - tfee, tfee,
                                     mout, min, min - mfee, mfee,
                                     mout - tin, currencyB, setPartiallyExecuted tout timeNow st, setFullyExecuted )
                            else let tout = tOutAmtLeft
                                     tin  = tInAmtExpects
                                     tfee = calcFeeCents defaultExchangeFee tin
                                     min  = tout
                                     mfee = calcFeeCents defaultExchangeFee min
                                     mout = multAmt (1 / mDirRatio) min
                                     st = exchangeOrderStatus mOrder
                                in ( tout, tin, tin - tfee, tfee,
                                     mout, min, min - mfee, mfee,
                                     mout - tin, currencyB, setFullyExecuted, setPartiallyExecuted mout timeNow st )
                $(logInfo) $ "U out: " <> (pack . show) userFinalOut <> "; in: " <> (pack . show) userFinalIn <> " ; fee: " <> (pack . show) userFinalFee
                $(logInfo) $ "M out: " <> (pack . show) matchFinalOut <> " ; in: " <> (pack . show) matchFinalIn <> " ; fee: " <> (pack . show) matchFinalFee
                $(logInfo) $ "Diff: " <> (pack . show) diffProfit
                res <- runDB $ do
                    -- Update order statuses and data
                    update tOrderId userOrderUpdates
                    update mOrderId matchOrderUpdates
                    -- Update users balances
                    update tWalletInId [ UserWalletAmountCents +=. userFinalIn ]
                    update mWalletInId [ UserWalletAmountCents +=. matchFinalIn ]
                    -- Record transaction details
                    -- First create transaction reasons
                    tRId <- insert $ WalletTransactionReason userWInId
                    mRId <- insert $ WalletTransactionReason matchWInId
                    -- Make chronological logs
                    -- Wallet balances
                    tTransRec <- insert $ WalletBalanceTransaction userWInId (ExchangeExchange userFinalIn) tRId tWalletInBalance timeNow
                    mTransRec <- insert $ WalletBalanceTransaction matchWInId (ExchangeExchange matchFinalIn) mRId mWalletInBalance timeNow
                    -- Order execution
                    tExecRec <- insert $ ExchangeOrderExecution tOrderId timeNow mRId tRId (tOutAmtLeft == userFinalOut) userFinalOut matchFinalOut userFinalFee
                    mExecRec <- insert $ ExchangeOrderExecution mOrderId timeNow tRId mRId (mOutAmtLeft == matchFinalOut) matchFinalOut userFinalOut matchFinalFee
                    -- Save fee data
                    tFeeProfitRec <- insert $ InnerProfitRecord tRId currencyB userFinalFee ExchangeFee
                    mFeeProfitRec <- insert $ InnerProfitRecord mRId currencyA matchFinalFee ExchangeFee
                    $(logInfo) $ "PROFIT +++ " <> (pack . show) diffProfit <> " " <> currSign currencyB
                    diffProfitRec <- if diffProfit > 0
                        then do
                            diffProfitId <- insert $ InnerProfitRecord mRId currencyA diffProfit ExchangeDiff
                            return [ diffProfitId ]
                        else return []
                    let profitRec = tFeeProfitRec : mFeeProfitRec : diffProfitRec
                    let res' = ( tOutAmtLeft == userFinalOut, tRId, mRId, tTransRec, mTransRec, tExecRec, mExecRec, profitRec )
                    return res'
                if tOutAmtLeft <= mInAmtExpects
                    then return $ res : acc
                    else do
                        target' <- fromJust <$> (runDB . get) tOrderId
                        exchangeOrders (Entity tOrderId target') mRest (res : acc)

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


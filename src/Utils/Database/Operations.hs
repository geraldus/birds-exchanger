{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}
module Utils.Database.Operations where

import           Import.NoFoundation

import           Local.Params           ( defaultExchangeFee )
import           Local.Persist.Currency ( Currency (..) )
import           Local.Persist.Exchange ( ExchangeOrderStatus (..),
                                          ExchangePair (..), ProfitType (..) )
import           Local.Persist.Wallet   ( TransactionTypePlain (..),
                                          WalletTransactionType (..) )
import           Utils.Money


saveAndExecuteOrder
    ::  ( MonadIO m
        , MonadLogger m
        , PersistStoreWrite backend
        , PersistQueryRead backend
        , PersistUniqueRead backend
        , BaseBackend backend ~ SqlBackend )
    => UserId
    -> AmountCents
    -> Currency
    -> UTCTime
    -> (UserWallet -> OrderCheck)
    -> ReaderT backend m OrderInsertionDbData
saveAndExecuteOrder client a c t withWalletCheck = do
    wallet <- getBy404 $ UniqueWallet client c
    let orderCheck = withWalletCheck (entityVal wallet)
    case orderCheck of
        OrderCheckErrors es -> return $ NoInsertion es
        OrderCheckSuccess withReasonOrder -> do
            (orderSaveTransactionD, o) <- saveOrder wallet a t withReasonOrder
            let saveData = (o, orderSaveTransactionD)
            exeData <- executeSavedOrder o t
            return $ Insertion saveData exeData

saveOrder
    ::  ( MonadIO m
        , MonadLogger m
        , PersistStoreWrite backend
        , PersistQueryRead backend
        , PersistUniqueRead backend
        , BaseBackend backend ~ SqlBackend )
    => Entity UserWallet
    -> AmountCents
    -> UTCTime
    -> (WalletTransactionReasonId -> ExchangeOrder)
    -> ReaderT backend m (TransactionD, Entity ExchangeOrder)
saveOrder w a time withReasonOrder = do
    r <- newWalletReason (entityKey w)
    t <- decreaseUserWalletBalance w r a mkType time
    let o = withReasonOrder r
    oid <- insert o
    return (TransactionD t r, Entity oid o)
  where mkType t = (ExchangeFreeze t, OrderCreation)

executeSavedOrder
    ::  ( MonadIO m
        , MonadLogger m
        , PersistStoreWrite backend
        , PersistQueryRead backend
        , PersistUniqueRead backend
        , BaseBackend backend ~ SqlBackend )
    => Entity ExchangeOrder
    -> UTCTime
    -> ReaderT backend m [ OrderExecutionData ]
executeSavedOrder o t = do
    ms <- findMatchingOrders o
    executeExchange o ms t []

executeExchange
    ::  ( MonadIO m
        , MonadLogger m
        , PersistStoreWrite backend
        , PersistQueryRead backend
        , PersistUniqueRead backend
        , BaseBackend backend ~ SqlBackend )
    => Entity ExchangeOrder
    -> [ Entity ExchangeOrder ]
    -> UTCTime
    -> [ OrderExecutionData ]
    -> ReaderT backend m [ OrderExecutionData ]
executeExchange _ [] _ a = pure a
executeExchange target (match:rest) time acc' = do
    let (Entity tId t) = target
    let (Entity _ m) = match
    let (_, tPair, tK, _, _) = orderParams t
    let (_, mPair, mK, _, _) = orderParams m
    when (mPair /= flipPair tPair) $
        error "Impossible happened!  Order pairs not match"
    when (wrongRatioCase tK mK tPair) $
        error "Impossible happened!  Wrong ratio condition"
    (targetClosed, ed, tUpdated) <- execute
    let acc = ed : acc'
    if targetClosed
        then return acc
        else executeExchange (Entity tId tUpdated) rest time acc
  where

    execute = do
        let (Entity tid t) = target
        let (Entity mid m) = match
        let mPair = exchangeOrderPair m
        let tclient = exchangeOrderUserId t
        let mclient = exchangeOrderUserId m
        let (tCurrencyIn, mCurrencyIn) = unPairCurrency mPair
        let p = exchangeParams t m
        ws@(tWallet, mWallet) <- getWallets404
                (tclient, tCurrencyIn) (mclient, mCurrencyIn)
        (tReason, mReason) <- mkReasons ws
        let tWalletReason = (tWallet, tReason)
        let mWalletReason = (mWallet, mReason)
        let tS = exchangeOrderStatus t
        let mS = exchangeOrderStatus m
        ((tStatus, tExecution, tP), (_, mExecution, mP), diffProfit) <-
                mkExchange (tid, tS) (mid, mS) tReason mReason p
        (tProfit, mProfit) <- saveFee
                (tReason, tCurrencyIn, tP) (mReason, mCurrencyIn, mP)
        let (tIn, tOut, tFee, tClosed) = tP
        let (mIn, _, mFee, _) = mP
        let tInc = (tWalletReason, tIn - tFee)
        let mInc = (mWalletReason, mIn - mFee)
        (tBalance, mBalance) <- increaseBalances tInc mInc
        mayDiffProfit <- saveDiffProfit mReason mCurrencyIn diffProfit
        let tData = ExecutionD
                tExecution (TransactionD tBalance tReason) tProfit mayDiffProfit
        let mData = ExecutionD
                mExecution (TransactionD mBalance mReason) mProfit Nothing
        let tLeft' = exchangeOrderAmountLeft t
        let tUpdated = t
                { exchangeOrderStatus = tStatus
                , exchangeOrderAmountLeft = if tClosed then 0 else tLeft' - tOut
                , exchangeOrderIsActive = tClosed
                }
        return (tClosed, (tData, mData), tUpdated)

    getWallets404 (c1, cur1) (c2, cur2)= do
        w1 <- getBy404 $ UniqueWallet c1 cur1
        w2 <- getBy404 $ UniqueWallet c2 cur2
        return (w1, w2)

    mkReasons (w1, w2) = (,)
        <$> newWalletReason (entityKey w1)
        <*> newWalletReason (entityKey w2)

    mkExchange (o1, s1) (o2, s2) r1 r2 params = do
        let (p1, p2, dp) = params
        let (in1, out1, fee1, closed1) = p1
            (in2, out2, fee2, closed2) = p2
        let (u1, us1) = updates closed1 out1 s1
            (u2, us2) = updates closed2 out2 s2
        let e1 = ExchangeOrderExecution o1 time r2 r1 closed1 out1 in1 fee1
            e2 = ExchangeOrderExecution o2 time r1 r2 closed2 out2 in2 fee2
        update o1 u1
        update o2 u2
        e1id <- insert e1
        e2id <- insert e2
        return
            ((us1, Entity e1id e1, p1), (us2, Entity e2id e1, p2), dp)
      where
        updates closed x s'
            | closed =
                let s = Executed time
                    u = [ ExchangeOrderStatus =. s
                        , ExchangeOrderAmountLeft =. 0
                        , ExchangeOrderIsActive =. not closed ]
                in (u, s)
            | otherwise =
                let s = partial s' x
                    u = [ ExchangeOrderStatus =. s
                        , ExchangeOrderAmountLeft -=. x
                        , ExchangeOrderIsActive =. not closed ]
                in (u, s)
        partial (PartiallyExecuted _ p) x =  PartiallyExecuted time (p + x)
        partial _ x                       = PartiallyExecuted time x

    saveFee (r1, c1, p1) (r2, c2, p2) = do
        let (_, _, f1, _) = p1
        let (_, _, f2, _) = p2
        let i1 = InnerProfitRecord r1 c1 f1 ExchangeFee
        let i2 = InnerProfitRecord r2 c2 f2 ExchangeFee
        i1id <- insert i1
        i2id <- insert i2
        return (Entity i1id i1, Entity i2id i2)

    increaseBalances
        ((w1, r1), in1) ((w2, r2), in2) = do
            t1 <- addUserWalletBalance w1 r1 in1 mkType time
            t2 <- addUserWalletBalance w2 r2 in2 mkType time
            return (t1, t2)
      where mkType t = (ExchangeExchange t, OrderExchange)

    saveDiffProfit r c dp
        | dp < 0 =
            error "Impossible happened: Diff profit is negative"
        | dp > 0 = do
            let tr = InnerProfitRecord r c dp ExchangeDiff
            insert tr >>= \x -> pure . Just . Entity x  $ tr
        | otherwise = pure Nothing

    wrongRatioCase tk mk = dirPairMatch (mk > tk) (mk < tk)

findMatchingOrders
    ::  ( MonadIO m
        , PersistStoreWrite backend
        , PersistQueryRead backend
        , BaseBackend backend ~ SqlBackend )
    => Entity ExchangeOrder
    -> ReaderT backend m [ Entity ExchangeOrder ]
findMatchingOrders order = do
    let (Entity orderId o) = order
    let opair = exchangeOrderPair o
    let ratio = exchangeOrderNormalizedRatio o
    let user  = exchangeOrderUserId o
    let rnorm = exchangeOrderRatioNormalization o
    let (cond, ord) = if defPairDir opair == opair
            then ((<=.), Asc)
            else ((>=.), Desc)
    selectList
        [ ExchangeOrderPair ==. flipPair opair
        , ExchangeOrderUserId !=. user
        , ExchangeOrderIsActive ==. True
        , ExchangeOrderAmountLeft >. 0
        , ExchangeOrderId !=. orderId
        , ExchangeOrderRatioNormalization ==. rnorm
        , ExchangeOrderNormalizedRatio `cond` ratio ]
        [ ord ExchangeOrderNormalizedRatio, Asc ExchangeOrderCreated ]

-- | Add @amount@ to @wallet@ query.
-- Creates and stores 'WalletBalanceTransaction'.
addUserWalletBalance ::
       (MonadIO m)
    => Entity UserWallet
    -> WalletTransactionReasonId
    -> Int
    -> (Int -> (WalletTransactionType, TransactionTypePlain))
    -> UTCTime
    -> SqlPersistT m (Entity WalletBalanceTransaction)
addUserWalletBalance wallet reason amount mkType time = do
    let (Entity wid w) = wallet
        before = userWalletAmountCents w
        (typ, typPlain) = mkType amount
        t = WalletBalanceTransaction
                wid typ reason before time (Just typPlain)
    update wid [ UserWalletAmountCents +=. amount ]
    tid <- insert t
    return (Entity tid t)

-- | Same as 'addUserWalletBalance' but expected to be used when
-- it is required to extract from wallet POSITIVE values.  Negates amount @a@
-- under the hood
decreaseUserWalletBalance ::
       (MonadIO m)
    => Entity UserWallet
    -> WalletTransactionReasonId
    -> PositiveAmount
    -> (Int -> (WalletTransactionType, TransactionTypePlain))
    -> UTCTime
    -> SqlPersistT m (Entity WalletBalanceTransaction)
decreaseUserWalletBalance w t a = addUserWalletBalance w t (negate a)


newWalletReason
    :: ( MonadIO m
       , PersistStoreWrite backend
       , BaseBackend backend ~ SqlBackend)
    => Key UserWallet
    -> ReaderT backend m (Key WalletTransactionReason)
newWalletReason w = insert $ WalletTransactionReason w


-- | Calculate exchange results for given orders
exchangeParams
    :: ExchangeOrder
    -> ExchangeOrder
    -> (TargetParams, MatchParams, DiffProfit)
exchangeParams target match =
    let (_, _, _, tRatio, tExpected) = orderParams target
        (mLeft, _, _, mRatio, _) = orderParams match
        (tIn, mOut) =  if tExpected < mLeft
            then (tExpected, tExpected)
            else (mLeft, mLeft)
        tR = 1 / tRatio
        mR = mRatio
        tOut = multiplyCents tR tIn
        mIn  = multiplyCents mR mOut
        tFee = calcFeeCents defaultExchangeFee tIn
        mFee = calcFeeCents defaultExchangeFee mIn
        profit = tOut - mIn
        (tClosed, mClosed)
            | tExpected < mLeft = (True, False)
            | tExpected == mLeft = (True, True)
            | otherwise = (False, True) -- tExpected > mLeft
    in ((tIn, tOut, tFee, tClosed), (mIn, mOut, mFee, mClosed), profit)


-- | Extract order parameters meaningful for exchange calculation
orderParams
    :: ExchangeOrder
    -> (AmountLeft, ExchangePair, NormalizedRatio, DirectRatio, AmountExpected)
orderParams ExchangeOrder{..} =
    let l = exchangeOrderAmountLeft
        p = exchangeOrderPair
        k = exchangeOrderNormalizedRatio
        r = pairRatioByNormalizedRatio p k
        e = multiplyCents r l
    in (l, p, k, r, e)

mkNewOrderData
    :: UserId
    -> AmountCents
    -> NormalizedRatio
    -> ExchangePair
    -> FeeCents
    -> UTCTime
    -> WalletTransactionReasonId
    -> ExchangeOrder
mkNewOrderData u a r p f time =
    ExchangeOrder u p a a (defPairDir p) r f time (Created time) True

-- | Select first option if default exchnage direction matches pair @p@.
dirPairMatch :: a -> a -> ExchangePair -> a
dirPairMatch equals notEquals p =
    if defPairDir p == p then equals else notEquals


-- ## Helper Types

data OrderCheck
    = OrderCheckErrors [ LabeledError ]
    | OrderCheckSuccess (WalletTransactionReasonId -> ExchangeOrder)

-- | Transaction and its reason
data TransactionD = TransactionD
        (Entity WalletBalanceTransaction) WalletTransactionReasonId

-- | Details of income operation during order execution
data ExecutionD = ExecutionD
        (Entity ExchangeOrderExecution)
        -- ^ Execution record
        TransactionD
        -- ^ Transaction and its Reason for order's creator IN-wallet
        (Entity InnerProfitRecord)
        -- ^ Exchange fee record
        (Maybe (Entity InnerProfitRecord))
        -- ^ Possible exchange outcome when ratios are different

data OrderInsertionDbData
    = NoInsertion [ LabeledError ]
    | Insertion OrderSaveData [ OrderExecutionData ]

type OrderSaveData = (Entity ExchangeOrder, TransactionD)

type OrderExecutionData = (ExecutionD, ExecutionD)

type AmountCents = Int

type AmountLeft = Int

type AmountExpected = Int

type NormalizedRatio = Double

type DirectRatio = Double

type TargetAmountCents = Int

type FeeCents = Int

type PositiveAmount = Int

type LabeledError = (Text, Text)

type TargetIn = Int

type TargetOut = Int

type TargetInFee = Int

type TargetClosed = Bool

type MatchIn = Int

type MatchOut = Int

type MatchInFee = Int

type MatchClosed = Bool

type TargetParams = (TargetIn, TargetOut, TargetInFee, TargetClosed)

type MatchParams = (MatchIn, MatchOut, MatchInFee, MatchClosed)

type DiffProfit = Int

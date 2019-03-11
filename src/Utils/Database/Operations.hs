{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}
module Utils.Database.Operations where

import           Import.NoFoundation

import           Local.Params           ( defaultExchangeFee )
import           Local.Persist.Currency ( Currency (..) )
import           Local.Persist.Exchange ( ExchangeOrderStatus (..),
                                          ExchangePair (..), ProfitType (..) )
import           Local.Persist.Wallet   ( WalletTransactionType (..) )
import           Utils.Money


saveAndExecuteOrder
    ::  ( MonadIO m
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
            reason <- newWalletReason (entityKey wallet)
            freeze <- decreaseUserWalletBalance
                    wallet reason a ExchangeFreeze t
            let orderSaveTransactionD = (freeze, reason)
            let order = withReasonOrder reason
            orderId <- insert order
            let o = Entity orderId order
            let orderSaveData = (o, orderSaveTransactionD)
            executionData <- executeSavedOrder o wallet
            error "123"

executeSavedOrder
    ::  ( MonadIO m
        , PersistStoreWrite backend
        , PersistQueryRead backend
        , BaseBackend backend ~ SqlBackend )
    => Entity ExchangeOrder
    -> Entity UserWallet
    -> ReaderT backend m OrderExecutionData
executeSavedOrder o w = do
    ms <- findMatchingOrders o
    error "!23"

executeExchange
    ::  ( MonadIO m
    , PersistStoreWrite backend
    , PersistQueryRead backend
    , BaseBackend backend ~ SqlBackend )
    => Entity ExchangeOrder
    -> Entity ExchangeOrder
    -> ( Entity ExchangeOrder, [ OrderExecutionData ] )
    -> ReaderT backend m ( Entity ExchangeOrder, [ OrderExecutionData ] )
executeExchange target match acc = do
    let (Entity _ t) = target
    let (Entity _ m) = match
    let (tLeft, tPair, tK, tRatio, tExpected) = orderParams t
    let (mLeft, mPair, mK, mRatio, mExpected) = orderParams m
    when (mPair /= flipPair tPair) $
        error "Impossible happened!  Order pairs not match"
    when (wrongRatio tPair tK mK) $
        error "Impossible happened!  Wrong ratio condition"
    error "execution step"
  where
    orderParams ExchangeOrder{..} =
        let l = exchangeOrderAmountLeft
            p = exchangeOrderPair
            k = exchangeOrderNormalizedRatio
            r = pairRatioByNormalizedRatio p k
            e = multiplyCents r l
        in (l, p, k, r, e)
    wrongRatio tp tk mk =
        if defPairDir tp == tp then mk > tk else mk < tk

findMatchingOrders
    ::  ( MonadIO m
        , PersistStoreWrite backend
        , PersistQueryRead backend
        , BaseBackend backend ~ SqlBackend )
    => Entity ExchangeOrder
    -> ReaderT backend m [ Entity ExchangeOrder ]
findMatchingOrders o = do
    let (Entity _ order) = o
    let opair = exchangeOrderPair order
    let ratio = exchangeOrderNormalizedRatio order
    let user  = exchangeOrderUserId order
    let (cond, ord) = if defPairDir opair == opair
            then ((<=.), Asc)
            else ((>=.), Desc)
    selectList
        [ ExchangeOrderPair ==. flipPair opair
        , ExchangeOrderUserId !=. user
        , ExchangeOrderNormalizedRatio `cond` ratio ]
        [ ord ExchangeOrderNormalizedRatio, Asc ExchangeOrderCreated ]

decreaseUserWalletBalance
    :: ( MonadIO m
    , PersistStoreWrite backend
    , BaseBackend backend ~ SqlBackend)
    => Entity UserWallet
    -> WalletTransactionReasonId
    -> PositiveAmount
    -> (Int -> WalletTransactionType)
    -> UTCTime
    -> ReaderT backend m (Entity WalletBalanceTransaction)
decreaseUserWalletBalance = addUserWalletBalance

addUserWalletBalance
    :: ( MonadIO m
    , PersistStoreWrite backend
    , BaseBackend backend ~ SqlBackend)
    => Entity UserWallet
    -> WalletTransactionReasonId
    -> Int
    -> (Int -> WalletTransactionType)
    -> UTCTime
    -> ReaderT backend m (Entity WalletBalanceTransaction)
addUserWalletBalance wallet reason amount mkType time = do
    let (Entity wid w) = wallet
        before = userWalletAmountCents w
        t = WalletBalanceTransaction
                wid (mkType amount) reason before time
    update wid [ UserWalletAmountCents +=. amount ]
    tid <- insert t
    return (Entity tid t)

newWalletReason
    :: ( MonadIO m
       , PersistStoreWrite backend
       , BaseBackend backend ~ SqlBackend)
    => Key UserWallet
    -> ReaderT backend m (Key WalletTransactionReason)
newWalletReason w = insert $ WalletTransactionReason w



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

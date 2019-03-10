{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}
module Utils.Database.Operations where

import           Import.NoFoundation

import           Local.Persist.Currency ( Currency (..) )
import           Local.Persist.Exchange ( ExchangeOrderStatus (Created),
                                          ExchangePair (..) )
import           Local.Persist.Wallet   ( WalletTransactionType (..) )
import           Utils.Money


type AmountCents = Int

type NormalizedRatio = Double

type TargetAmountCents = Int

type FeeCents = Int

type PositiveAmount = Int

type LabeledError = (Text, Text)


data OrderCheck
    = OrderCheckErrors [ LabeledError ]
    | OrderCheckSuccess (WalletTransactionReasonId -> ExchangeOrder)


    -- | Transaction and its reason
data TransactionD = TransactionD
        (Entity WalletBalanceTransaction) (Entity WalletTransactionReason)

-- | Details of income operation during order execution
data ExecutionD = ExecutionD
        (Entity ExchangeOrderExecution) TransactionD (Entity InnerProfitRecord)

type OrderSaveData = (Entity ExchangeOrder, TransactionD)
type OrderExecutionData = [ (ExecutionD, ExecutionD) ]

data OrderInsertionDbData
    = NoInsertion [ LabeledError ]
    | Insertion OrderSaveData OrderExecutionData

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

newWalletReason
    :: ( MonadIO m
       , PersistStoreWrite backend
       , BaseBackend backend ~ SqlBackend)
    => Key UserWallet
    -> ReaderT backend m (Key WalletTransactionReason)
newWalletReason w = insert $ WalletTransactionReason w


mkNewOrderData
    :: UserId
    -> AmountCents
    -> NormalizedRatio
    -> ExchangePair
    -> TargetAmountCents
    -> FeeCents
    -> UTCTime
    -> WalletTransactionReasonId
    -> ExchangeOrder
mkNewOrderData u a r p t f time =
    ExchangeOrder u p a a (defPairDir p) r f time (Created time) True


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
decreaseUserWalletBalance wallet reason amount mkType time = do
    let (Entity wid w) = wallet
        before = userWalletAmountCents w
        t = WalletBalanceTransaction
                wid (mkType $ negate amount) reason before time
    update wid [ UserWalletAmountCents -=. amount ]
    tid <- insert t
    return (Entity tid t)

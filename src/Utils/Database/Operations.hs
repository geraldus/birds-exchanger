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

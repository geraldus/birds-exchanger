{-# LANGUAGE GADTs #-}
module Utils.Database.Operations where

import           Import.NoFoundation

import           Local.Persist.Exchange ( ExchangeOrderStatus (Created),
                                          ExchangePair (..) )
import           Utils.Money            ( defPairDir )


type AmountCents = Int

type NormalizedRatio = Double

type TargetAmountCents = Int

type FeeCents = Int


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

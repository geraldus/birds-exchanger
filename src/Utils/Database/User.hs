{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Utils.Database.User where

import           Import.NoFoundation    hiding ( isNothing, on, (==.), (>=.),
                                          (\\) )
import           Local.Params           ( defaultWalletCurrencies )
import           Local.Persist.Currency ( Currency, currencyCodeT' )
import           Local.Persist.Exchange ( ExchangePair, exchangePairUnsafe )
import           Local.Persist.Wallet   ( DepositRequestStatus (OperatorAccepted),
                                          TransactionTypePlain (ParaMiningAccrual),
                                          WithdrawalStatus (WsNew) )

import           Data.Aeson             ( ToJSON (..) )
import           Data.List              ( (\\) )
import           Data.Time.Clock.POSIX  ( utcTimeToPOSIXSeconds )
import           Database.Esqueleto


-- * Database Queries
--
-- All queries are written with `esqueleto` language and (mostly)
-- are type-safe queries.

-- | Get list of user wallets
getUserWallets :: (MonadIO m) => UserId -> SqlPersistT m [Ent Wal]
getUserWallets userId = select . from $ \w -> do
        where_ (w ^. UserWalletUserId ==. val userId)
        return w

-- | Get last wallet transaction which lead to paramining transfer
-- if present.
-- Return type is a list, which is empty when no para-transaction present
-- and exactly last para-transaction otherwise.  Uses `LIMIT 1` internally
-- (via `esqueleto`\'s 'limit' function).
getUserWalletLastParaTransaction ::
       (MonadIO m) => Ent Wal -> SqlPersistT m (Maybe (Ent BTrans))
getUserWalletLastParaTransaction (Entity wid w) = do
    extr <- selectLastExchangeTransaction (limit 1)
    wreqtr <- selectLastActiveWithdrawalRequestTransaction (limit 1)
    dreqtr <- selectLastAcceptedDepositRequestTransaction (limit 1)
    let ts = extr <> wreqtr <> dreqtr
    return $ case ts of
        []  -> Nothing
        t:_ -> Just (foldr maxUTCTime t ts)
  where
    selectLastExchangeTransaction limitQ = select . from $
        \(o, exec, r, t) -> do
            let pairs = defaultExchangePairsOf (userWalletCurrency w)
            where_ (
                -- order constraints
                (o ^. ExchangeOrderUserId ==. val (userWalletUserId w))
                &&. (o ^. ExchangeOrderPair `in_` valList pairs)
                -- order execution constraints
                &&. (o ^. ExchangeOrderId
                    ==. exec ^. ExchangeOrderExecutionOrderId)
                --  transaction reason constraints
                &&. (exec ^. ExchangeOrderExecutionOutWalletTransactionReasonId
                    ==. r ^. WalletTransactionReasonId)
                &&. (r ^. WalletTransactionReasonId
                    ==. t ^. WalletBalanceTransactionWalletTransactionReasonId)
                )
            orderBy [desc (t ^. WalletBalanceTransactionTime)]
            limitQ
            return t

    selectLastActiveWithdrawalRequestTransaction limitQ = select . from $
        \(wr, r, t) -> do
            where_ (
                (wr ^. WithdrawalRequestWalletId ==. val wid)
                &&. (wr ^. WithdrawalRequestStatus ==. val WsNew)
                &&. (wr ^. WithdrawalRequestAccepted ==. nothing)
                &&. (wr ^. WithdrawalRequestWalletTransactionReasonId
                        ==. r ^. WalletTransactionReasonId )
                &&. (r ^. WalletTransactionReasonId
                        ==. t ^. WalletBalanceTransactionWalletTransactionReasonId)
                )
            orderBy [desc (t ^. WalletBalanceTransactionTime)]
            limitQ
            return t

    selectLastAcceptedDepositRequestTransaction limitQ = do
        res <- select . from $
            \(acd, dr, r, t) -> do
                where_ (
                    (acd ^. AcceptedDepositDepositRequestId
                            ==. dr ^. DepositRequestId)
                    &&. (dr ^. DepositRequestUserId ==. val (userWalletUserId w))
                    &&. (dr ^. DepositRequestCurrency
                            ==. val (userWalletCurrency w))
                    &&. (dr ^. DepositRequestArchived ==. val False)
                    -- reason constraints
                    &&. (acd ^. AcceptedDepositWalletTransactionReasonId
                            ==. r ^. WalletTransactionReasonId )
                    &&. (r ^. WalletTransactionReasonId
                            ==. t ^. WalletBalanceTransactionWalletTransactionReasonId)
                    )
                orderBy [desc (t ^. WalletBalanceTransactionTime)]
                limitQ
                return (t, dr)
        return $ fst <$> filter isAcceptedDeposit res

    maxUTCTime t1 t2 = if transTime t1 > transTime t2 then t1 else t2

    transTime = walletBalanceTransactionTime . entityVal

    isAcceptedDeposit (_, Entity _ depreq) = case depositRequestStatus depreq of
        OperatorAccepted _ -> True
        _                  -> False

-- | Get active exchange orders for given wallet.
-- Useful for calculating amount cents being held within orders.
getUserWalletActiveOrders ::
        (MonadIO m) => Ent Wal -> SqlPersistT m [Ent ExOrd]
getUserWalletActiveOrders (Entity wid wallet) = select $ from $
    \(w `InnerJoin` u `LeftOuterJoin` o) -> do
        on (u ^. UserId ==. o ^. ExchangeOrderUserId)
        on (w ^. UserWalletUserId ==. u ^. UserId)
        let currency = userWalletCurrency wallet
        let pairCandidates =
                    defaultWalletCurrencies \\ [ currency ]
        let pairs = map (exchangePairUnsafe currency) pairCandidates
        where_ (
            (o ^. ExchangeOrderPair `in_` valList pairs)
            &&. (w ^. UserWalletId ==. val wid)
            &&. (o ^. ExchangeOrderIsActive  ==. val True)
            )
        return o

-- | Get withdrawal requests for given wallet that are not executed yet.
-- Useful for frozen amount cents sum calculation.
getUserWalletActiveWithdrawal ::
        (MonadIO m) => Ent Wal -> SqlPersistT m [Ent WReq]
getUserWalletActiveWithdrawal (Entity wid _) = select $ from $
    \w -> do
        where_ (
            (w ^. WithdrawalRequestWalletId ==. val wid)
            &&. isNothing (w ^. WithdrawalRequestAccepted)
            &&. (w ^. WithdrawalRequestStatus ==. val WsNew)
            )
        return w


-- * Useful type name shortands.

type Ent = Entity

type Wal = UserWallet

type ExOrd = ExchangeOrder

type WReq = WithdrawalRequest

type BTrans = WalletBalanceTransaction


-- * Statistics

data UserWalletStatsResponse
    = NoSuchUser UserId
    | UserWalletData [WalletData]

-- | Represent wallet balance statistics
data WalletData = WalletData
    { walletDataWallet              :: Entity UserWallet
    , walletDataOrdersCents         :: Int
    -- ^ actual amount of cents held within active orders, in other words
    -- how many available cents left in orders
    , walletDataWithdrawalCents     :: Int
    -- ^ actual amount of cents help within yet unexecuted withdrawals requests,
    -- in other words how many cents could be returned to balance if all
    -- of withdrawal requests will be cencelled
    , walletDataLastParaTransaction :: Maybe (Entity WalletBalanceTransaction)
    -- ^ last found balance transaction lead to paramining accounting
    }
    deriving Show

-- | Marshal database results to 'WalletData'.
foldUserWalletStats ::
        Ent Wal -> [Ent ExOrd] -> [Ent WReq] -> Maybe (Ent BTrans) -> WalletData
foldUserWalletStats wallet os rs trans = WalletData
    { walletDataWallet = wallet
    , walletDataOrdersCents = foldOrders os
    , walletDataWithdrawalCents = foldReqs rs
    , walletDataLastParaTransaction = trans
    }
  where
        foldOrders :: [Ent ExOrd] -> Int
        foldOrders = sum . map (exchangeOrderAmountLeft . entityVal)

        foldReqs :: [Ent WReq] -> Int
        foldReqs = sum . map (withdrawalRequestFrozenAmount . entityVal)

-- | JSON Representation
instance ToJSON WalletData where
    toJSON WalletData{..} = object
        [ "wallet" .= walletJSON
        , "orders" .= toJSON walletDataOrdersCents
        , "withdrawal" .= toJSON walletDataWithdrawalCents
        , "lastParaTransactionTime" .= utcTimestampJSON lastParaTime ]
      where
        walletId = entityKey walletDataWallet

        wallet = entityVal walletDataWallet

        walletJSON = object
            [ "id" .= toJSON walletId
            , "currency" .= toJSON
                    (toLower . currencyCodeT' . userWalletCurrency $ wallet)
            , "balance" .= toJSON (userWalletAmountCents wallet)
            , "lastParaTransactionTime" .= toJSON (
                        toUnixTimestampDouble
                        <$> userWalletLastParaTransaction wallet )
            ]

        lastParaTime = (walletBalanceTransactionTime . entityVal)
                <$> walletDataLastParaTransaction

        utcTimestampJSON = toJSON . (toUnixTimestampDouble <$>)

        toUnixTimestampDouble = toDouble . utcTimeToPOSIXSeconds

        toDouble :: Real a => a -> Double
        toDouble = realToFrac



-- let currency = userWalletCurrency wallet

defaultExchangePairsOf :: Currency -> [ExchangePair]
defaultExchangePairsOf currency = map (exchangePairUnsafe currency) candidates
  where candidates = defaultWalletCurrencies \\ [ currency ]

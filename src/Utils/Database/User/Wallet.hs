{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}
module Utils.Database.User.Wallet where

import           Import.NoFoundation    hiding ( isNothing, on, update, (+=.),
                                          (==.), (>=.), (\\) )

import           Local.Params           ( defaultParaMiningDelaySeconds,
                                          defaultWalletCurrencies )
import           Local.Persist.Currency ( Currency, ouroC, pzmC )
import           Local.Persist.Exchange ( ExchangePair (..),
                                          exchangePairUnsafe )
import           Local.Persist.Wallet   ( DepositRequestStatus (OperatorAccepted),
                                          TransactionTypePlain (..),
                                          WalletTransactionType,
                                          WithdrawalStatus (WsNew) )
import           Type.Money             ( Percent, mkPercent, percentToDouble )
import           Type.Wallet            ( WalletData (..) )
import           Utils.Type

import           Data.List              ( (\\) )
import           Data.Maybe             ( listToMaybe )
import           Data.Time.Clock        ( NominalDiffTime, addUTCTime,
                                          diffUTCTime )
import           Database.Esqueleto
import           Database.Persist       as P ( update, (+=.), (=.) )

-- * Database Queries
--
-- All queries are written with `esqueleto` language and (mostly)
-- are type-safe queries.

-- | Get list of user wallets
getUserWallets :: (MonadIO m) => UserId -> SqlPersistT m [Ent Wal]
getUserWallets userId = select . from $ \w -> do
        where_ (w ^. UserWalletUserId ==. val userId)
        orderBy [ asc (w ^. UserWalletId) ]
        return w

getUserWalletByCurrency ::
       MonadIO m
    => UserId -> Currency -> SqlPersistT m (Maybe (Ent Wal))
getUserWalletByCurrency userId currency =
    (maybeList <$>)
    <$> select . from $ \w -> do
        where_ (
                (w ^. UserWalletUserId ==. val userId)
            &&. (w ^. UserWalletCurrency ==. val currency)
            )
        limit 1
        return w
  where maybeList []      = Nothing
        maybeList (i : _) = Just i

getUserWalletsByCurrency ::
       MonadIO m
    => UserId -> [Currency] -> SqlPersistT m [Ent Wal]
getUserWalletsByCurrency userId currencies = select . from $ \w -> do
        where_ (
                (w ^. UserWalletUserId ==. val userId)
            &&. (w ^. UserWalletCurrency `in_` valList currencies)
            )
        orderBy [ asc (w ^. UserWalletId) ]
        return w

getOrCreateWalletDB ::
       MonadIO m
    => UserId
    -> Text
    -> Currency
    -> SqlPersistT m (Entity UserWallet)
getOrCreateWalletDB userId walletTextId currency = do
    time <- liftIO getCurrentTime
    let newWallet = UserWallet userId currency 0 walletTextId time Nothing
    eitherWallet <- insertBy newWallet
    return $ case eitherWallet of
        Left entity -> entity
        Right wid   -> Entity wid newWallet

-- | Query for last accepted deposit time
walletLastDepositParaTimeDB ::
       (MonadIO m) => Ent Wal -> SqlPersistT m (Maybe UTCTime)
walletLastDepositParaTimeDB (Entity wid w) = do
        accept <- select $ from $
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
                limit 1
                return  t
        return . listToMaybe $
            fmap (walletBalanceTransactionTime . entityVal) accept

-- | Query for last executed withdrawal time
walletLastWithdrawalParaTimeDB ::
       (MonadIO m) => Ent Wal -> SqlPersistT m (Maybe UTCTime)
walletLastWithdrawalParaTimeDB (Entity wid w) = do
    exec <- select $ from $
        \(acw, wr) -> do
            where_ (
                (acw ^. WithdrawalAcceptRequestId
                        ==. wr ^. WithdrawalRequestId)
                &&. (wr ^. WithdrawalRequestWalletId ==. val wid)
                )
            orderBy [desc (acw ^. WithdrawalAcceptTime)]
            limit 1
            return  acw
    return . listToMaybe $
        fmap (withdrawalAcceptTime . entityVal) exec


walletLastExchangeParaTimeDB ::
       (MonadIO m) => Ent Wal -> SqlPersistT m (Maybe UTCTime)
walletLastExchangeParaTimeDB (Entity wid w) = do
    let pairsIn = defaultExchangePairsOf (userWalletCurrency w)
    let pairsOut = defaultOppositeExchangePairsOf (userWalletCurrency w)
    let pairs = pairsIn <> pairsOut
    tr <- select . from $ \(o, exec, r, t) -> do
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
        limit 1
        return t
    return . listToMaybe $
        fmap (walletBalanceTransactionTime . entityVal) tr


walletLastParaTransactionTimeDB ::
       (MonadIO m) => Ent Wal -> SqlPersistT m (Maybe UTCTime)
walletLastParaTransactionTimeDB (Entity wid w) = do
    ptr <- select . from $ \t -> do
        where_ (
            (t ^. WalletBalanceTransactionWalletId ==. val wid)
            &&. (t ^. WalletBalanceTransactionPlainType
                    ==. val ParaMiningAccrual)
            )
        orderBy [desc (t ^. WalletBalanceTransactionTime)]
        limit 1
        return t
    return . listToMaybe $
        fmap (walletBalanceTransactionTime . entityVal) ptr


-- | Get wallet's lasts paraming time by quering for actual database
-- entities responsible to paramining accrual or paramining accrual
-- operation time
lastWalletStrictParaTimeDB ::
       (MonadIO m) => Ent Wal -> SqlPersistT m (Maybe UTCTime)
lastWalletStrictParaTimeDB wallet = do
    paraTime <- walletLastParaTransactionTimeDB wallet
    depTime <- walletLastDepositParaTimeDB wallet
    witTime <- walletLastWithdrawalParaTimeDB wallet
    exTime <- walletLastExchangeParaTimeDB wallet
    let times = [paraTime, depTime, witTime, exTime]
    return . listToMaybe $ sortBy descTime (catMaybes times)
  where
    descTime t1 t2 = if t1 < t2 then GT else LT

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

-- * Statistics

-- | Query database and collect user's wallet stats returning them
-- as 'WalletData'.
getUserWalletStatsDB :: MonadIO m => Entity UserWallet -> SqlPersistT m WalletData
getUserWalletStatsDB wal = do
    os   <- getUserWalletActiveOrders wal
    rs   <- getUserWalletActiveWithdrawal wal
    ptime <- lastWalletStrictParaTimeDB wal
    return $ foldUserWalletStats wal os rs ptime

-- | Marshal database results to 'WalletData'.
foldUserWalletStats ::
        Ent Wal -> [Ent ExOrd] -> [Ent WReq] -> Maybe UTCTime -> WalletData
foldUserWalletStats wallet os rs t = WalletData
    { walletDataWallet = wallet
    , walletDataOrdersCents = foldOrders os
    , walletDataWithdrawalCents = foldReqs rs
    , walletDataLastParaTime = t
    , walletDataParaminingRate = defMonthlyParamine $
        (userWalletCurrency . entityVal) wallet
    }
  where
        foldOrders :: [Ent ExOrd] -> Int
        foldOrders = sum . map (exchangeOrderAmountLeft . entityVal)

        foldReqs :: [Ent WReq] -> Int
        foldReqs = sum . map (withdrawalRequestFrozenAmount . entityVal)


defaultExchangePairsOf :: Currency -> [ExchangePair]
defaultExchangePairsOf currency = map (exchangePairUnsafe currency) candidates
  where candidates = defaultWalletCurrencies \\ [ currency ]

defaultOppositeExchangePairsOf :: Currency -> [ExchangePair]
defaultOppositeExchangePairsOf currency =
    map (flip exchangePairUnsafe currency) candidates
  where candidates = defaultWalletCurrencies \\ [ currency ]


-- | Add @amount@ to @wallet@ query.
-- Creates and stores 'WalletBalanceTransaction'.
addUserWalletBalance ::
        (MonadIO m)
        => Entity UserWallet
        -> WalletTransactionReasonId
        -> Int
        -> (Int -> (WalletTransactionType, TransactionTypePlain))
        -- ^ type generator, will be replaced with TransactionTypePlain value in
        -- next major release
        -> UTCTime
        -> SqlPersistT m (Entity WalletBalanceTransaction)
addUserWalletBalance wallet reason amount mkType time = do
    let (Entity wid w) = wallet
        before = userWalletAmountCents w
        (typ, typPlain) = mkType amount
        t = WalletBalanceTransaction
                wid typ reason before time typPlain
        -- update last para-time when needed
        -- update time even if no actual paramining will be accrued
        -- in such scenario un-accrued value will be lost
        -- to be able delay accrual we omit 'ParaMiningAccrual'
        -- transactions in paramining related operations
    let paraOpsTypes = [ DepositAccept, OrderExchange ]
        isParaOp = typPlain `elem` paraOpsTypes
        paraTimeUpdate =
            [ UserWalletLastParaTime P.=. Just time | isParaOp ]
        amountUpdate =
            [ UserWalletAmountCents P.+=. amount ]
    let updates = amountUpdate <> paraTimeUpdate
    P.update wid updates
    tid <- insert t
    return (Entity tid t)

-- | Same as 'addUserWalletBalance' but expected to be used when
-- it is required to extract from wallet POSITIVE values.  Negates amount @a@
-- under the hood
decreaseUserWalletBalance ::
        (MonadIO m)
        => Entity UserWallet
        -> WalletTransactionReasonId
        -> Int
        -> (Int -> (WalletTransactionType, TransactionTypePlain))
        -> UTCTime
        -> SqlPersistT m (Entity WalletBalanceTransaction)
decreaseUserWalletBalance w t a = addUserWalletBalance w t (negate a)

newWalletReason ::
        MonadIO m
        => Key UserWallet
        -> SqlPersistT m (Key WalletTransactionReason)
newWalletReason w = insert $ WalletTransactionReason w

-- * Utilities

{-# DEPRECATED getWallets404 "Since 0.6.13. Redirecting on missing wallet is confusing.  You should always handle this case." #-}
-- | Get wallet or redirect to 404 page
getWallets404 ::
        MonadIO m
        => (UserId, Currency)
        -> (UserId, Currency)
        -> SqlPersistT m (Entity UserWallet, Entity UserWallet)
getWallets404 (c1, cur1) (c2, cur2) = (,)
    <$> getWallet404 c1 cur1
    <*> getWallet404 c2 cur2

{-# DEPRECATED getWallet404 "Since 0.6.13. Redirecting on missing wallet is confusing.  You should always handle this case." #-}
getWallet404 ::
        MonadIO m => UserId -> Currency -> SqlPersistT m (Entity UserWallet)
getWallet404 user = getBy404 . UniqueWallet user


-- | Calculate paramining if conditions are met.
-- Takes in account 3 seconds paramining accrual delay.
currencyAmountPara :: UTCTime -> UTCTime -> Currency -> Int -> Maybe (Double, Double)
currencyAmountPara tnow t' c a =
    case defMonthlyParamine c of
        Nothing -> Nothing
        Just p' ->
            let p100 = percentToDouble p'
                p = p100 / 100
                delay = fromIntegral defaultParaMiningDelaySeconds :: Double
                t = addUTCTime (realToFrac delay) t'
                -- take in account paramining delay
                s = diffUTCTime tnow t
                v' = fromIntegral  a
                k_s = secondsParaminingRate p
                k = k_s ** nominalDiffTimeToSeconds s
                v = if tnow < t
                        then 0
                        else v' * (k - 1)
            in Just (v, k_s)

defMonthlyParamine :: Currency -> Maybe Percent
defMonthlyParamine c
    | c == pzmC = Just (mkPercent 12)
    | c == ouroC = Just (mkPercent 12)
    | otherwise = Nothing

monthSeconds :: Double
monthSeconds = 30 * 24 * 60 * 60

secondsParaminingRate :: Double -> Double
secondsParaminingRate p = (p + 1) ** (1 / monthSeconds)

nominalDiffTimeToSeconds :: NominalDiffTime -> Double
nominalDiffTimeToSeconds = realToFrac


isAcceptedDeposit :: Entity DepositRequest -> Bool
isAcceptedDeposit (Entity _ r) = case depositRequestStatus r of
    OperatorAccepted _ -> True
    _                  -> False

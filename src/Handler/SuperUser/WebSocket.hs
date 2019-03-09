{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
module Handler.SuperUser.WebSocket where

import           Import                 hiding ( Value (..), count, groupBy,
                                          isNothing, on, (==.), (||.) )

import           Local.Persist.Currency
import           Local.Persist.UserRole
import           Local.Persist.Wallet
import           Yesod.WebSockets

import qualified Data.Aeson             as A
import           Data.Ratio
import           Database.Esqueleto

default (Text, String)


getSuperUserWebSocketR :: Handler Html
getSuperUserWebSocketR =
    webSockets superUserWebSocket >> notFound


superUserWebSocket :: WebSocketsT Handler ()
superUserWebSocket = do
    sendTextData ("Welcome Super User!" :: Text)
    forever $ do
        t <- receiveData :: WebSocketsT Handler Text
        case t of
            "user count" -> liftHandler getUsersCount >>=
                send' . countEventToJson "User Count"
            "inner profit stats" -> (pairMaybeSum . pairMapCurrencyCode)
                    <$> liftHandler getInnerProfit
                    >>= send' . countsEventToJson "Inner Profit"
            "active deposit count" -> liftHandler getActiveDepositCount >>=
                send' . countEventToJson "Active Deposit Count"
            "accepted deposit count" -> liftHandler getAcceptedDepositCount >>=
                send' . countEventToJson "Accepted Deposit Count"
            "deposited money" -> do
                (c, inc, fee) <- (unzip3 . tripleMaybeSum . tripleMapCurrencyCode)
                        <$> liftHandler getDepositedMoney
                send' (countsEventToJson "Deposited Money" (zip c inc))
                send' (countsEventToJson "Deposit Fee" (zip c fee))
            "wallet stats" -> (pairMaybeSum . pairMapCurrencyCode)
                <$> liftHandler getWalletBalances
                >>= send' . countsEventToJson "Wallet Stats"
            "withdrawal stats" -> do
                (ncnt, nstats) <- liftHandler getActiveWithdrawalStats
                let ntstats = map (\(x, y, _) -> (currencyCodeT x, y)) nstats
                let nfstats = map (\(x, _, z) -> (currencyCodeT x, z)) nstats
                send' $ countEventToJson "Withdrawal New Count" ncnt
                send' $ countsEventToJson "Withdrawal New Amount Stats" ntstats
                send' $ countsEventToJson "Withdrawal New Frozen Stats" nfstats
                (ecnt, estats) <- liftHandler getAcceptedWithdrawalStats
                let etstats = map (\(x, y, _) -> (currencyCodeT x, y)) estats
                let efstats = map (\(x, _, z) -> (currencyCodeT x, z)) estats
                send' $ countEventToJson "Withdrawal Accepted Count" ecnt
                send' $ countsEventToJson "Withdrawal Accepted Transfer Stats" etstats
                send' $ countsEventToJson "Withdrawal Accepted Fee Stats" efstats
            _            -> sendTextData t
  where send' = sendTextData . decodeUtf8 . A.encode


data CountEvent = CountEvent
  { ceObjectType :: Text
  , ceValue      :: Int }

countEventToJson :: Text -> Int -> A.Value
countEventToJson typ n = A.object
  [ "type" A..= "count-event"
  , "object" A..= typ
  , "value" A..= n ]

countsEventToJson :: Text -> [(Text, Int)] -> A.Value
countsEventToJson typ vs = A.object
  [ "type" A..= "count-event"
  , "object" A..= typ
  , "value" A..= object (map (uncurry (A..=)) vs) ]


getUsersCount :: Handler Int
getUsersCount =
    fmap take1st <$> runDB $ select $ from (\u -> do
        where_ (u ^. UserRole ==. val Client)
        return $ count (u ^. UserId))


getActiveOrdersCount :: Handler Int
getActiveOrdersCount = error "!23"

getOrderExecutionsCount :: Handler Int
getOrderExecutionsCount = error "123"


getInnerProfit :: Handler [(Currency, Maybe Rational)]
getInnerProfit = fmapUnValuePair
    <$> runDB $ select $ from (\ip -> do
            let c = ip ^. InnerProfitRecordCurrency
            groupBy c
            return (c, sum_ (ip ^. InnerProfitRecordAmountCents)))



{- DEPOSITS -}

getActiveDepositList :: Handler [ Entity DepositRequest ]
getActiveDepositList = runDB $ select $ from $ \d -> do
    let s = d ^. DepositRequestStatus
    where_ ({-s ==. val New ||. -}s ==. val ClientConfirmed)
    return d

getActiveDepositCount :: Handler Int
getActiveDepositCount = fmap take1st <$> (runDB . select . from) $ \d -> do
    let s = d ^. DepositRequestStatus
    where_ ({-s ==. val New ||. -}s ==. val ClientConfirmed)
    return (count $ d ^. DepositRequestId)

getAcceptedDepositCount :: Handler Int
getAcceptedDepositCount = fmap take1st <$> (runDB . select . from) $
    \(d `InnerJoin` a) -> do
        on (d ^. DepositRequestId ==. a ^. AcceptedDepositDepositRequestId)
        return (count $ d ^. DepositRequestId)

getDepositedMoney :: Handler [(Currency, Maybe Rational, Maybe Rational)]
getDepositedMoney = fmapUnValueTriple <$> (runDB . select . from) $
    \(d `InnerJoin` a) -> do
        on (d ^. DepositRequestId ==. a ^. AcceptedDepositDepositRequestId)
        let c = d ^. DepositRequestCurrency
        groupBy c
        let income = sum_ $ a ^. AcceptedDepositCentsRealIncome
        let fee = sum_ $ a ^. AcceptedDepositCentsActualFee
        return (c, income, fee)


{- WITHDRAWAL -}

getActiveWithdrawalStats :: Handler (Int, [ (Currency, Int, Int)])
getActiveWithdrawalStats = runDB $ do
    cnt <- fmap take1st <$> (select . from) $ \d -> do
        let s = d ^. WithdrawalRequestStatus
        where_ (s ==. val WsNew)
        return (count $ d ^. WithdrawalRequestId)
    stats <- fmap unValues <$> select $ from $
        \(r `InnerJoin` w) -> do
            on (r ^. WithdrawalRequestWalletId ==. w ^. UserWalletId)
            where_ (isNothing $ r ^. WithdrawalRequestAccepted)
            let c = w ^. UserWalletCurrency
            groupBy c
            let sa = sum_ (r ^. WithdrawalRequestCentsAmount)
            let sf = sum_ (r ^. WithdrawalRequestFrozenAmount)
            return (c, sa, sf)
    return (cnt, stats)
  where
    unValues
        :: [(Value Currency, Value (Maybe Rational), Value (Maybe Rational))]
        -> [(Currency, Int, Int)]
    unValues = map
        (\(x, y, z) -> (unValue x, (may0 . unValue) y, (may0 . unValue) z))

getAcceptedWithdrawalStats :: Handler (Int, [ (Currency, Int, Int) ])
getAcceptedWithdrawalStats = runDB $ do
    cnt <- fmap take1st <$> (select . from) $ \(r `InnerJoin` e) -> do
        on (r ^. WithdrawalRequestId ==. e ^. WithdrawalAcceptRequestId)
        return (count $ r ^. WithdrawalRequestId)
    stats <- fmap unValues <$> select $ from $
        \(w `InnerJoin` r `InnerJoin` e) -> do
            on (r ^. WithdrawalRequestId ==. e ^. WithdrawalAcceptRequestId)
            on (r ^. WithdrawalRequestWalletId ==. w ^. UserWalletId)
            let c = w ^. UserWalletCurrency
            groupBy c
            let st = sum_ (e ^. WithdrawalAcceptAmountTransfered)
            let sf = sum_ (e ^. WithdrawalAcceptActualFee)
            return (c, st, sf)
    return (cnt, stats)
  where
    unValues
        :: [(Value Currency, Value (Maybe Rational), Value (Maybe Rational))]
        -> [(Currency, Int, Int)]
    unValues = map
        (\(x, y, z) -> (unValue x, (may0 . unValue) y, (may0 . unValue) z))


{- WALLETS -}

getWalletBalances :: Handler [ (Currency, Maybe Rational) ]
getWalletBalances = fmapUnValuePair
    <$> runDB $ select $ from (\w -> do
            let c = w ^. UserWalletCurrency
            groupBy c
            return (c, sum_ (w ^. UserWalletAmountCents)))



{- UTILS -}

take1st :: Num p => [Database.Esqueleto.Value p] -> p
take1st []    = 0
take1st (x:_) = unValue x

-- unValuePair = map (\(a, b) -> (unValue a, unValue b))

fmapUnValuePair
    :: Handler [(Database.Esqueleto.Value a, Database.Esqueleto.Value b)]
    -> Handler [(a, b)]
fmapUnValuePair = fmap (map (\(a, b) -> (unValue a, unValue b)))

fmapUnValueTriple
    :: Handler [(Database.Esqueleto.Value a, Database.Esqueleto.Value b, Database.Esqueleto.Value c)]
    -> Handler [(a, b, c)]
fmapUnValueTriple = fmap (map (\(a, b, c) -> (unValue a, unValue b, unValue c)))


pairMapCurrencyCode :: [(Currency, b)] -> [(Text, b)]
pairMapCurrencyCode = map $ \(x,y) -> (currencyCodeT x, y)

tripleMapCurrencyCode :: [(Currency, b, b)] -> [(Text, b, b)]
tripleMapCurrencyCode = map $ \(x,y, z) -> (currencyCodeT x, y, z)

pairMaybeSum :: (Num n, RealFrac n) => [(a, Maybe n)] -> [(a, Int)]
pairMaybeSum = map $ \(x, y) -> (x, may0 y)

tripleMaybeSum :: (Num n, RealFrac n) => [(a, Maybe n, Maybe n)] -> [(a, Int, Int)]
tripleMaybeSum = map $ \(x, y, z) -> (x, may0 y, may0 z)

may0 :: (Num n, RealFrac n) => Maybe n -> Int
may0 = maybe 0 round

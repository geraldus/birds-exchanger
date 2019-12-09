{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
module Handler.SuperUser.WebSocket where

import           Import                                       hiding
                                                                ( Value (..),
                                                                count, groupBy,
                                                                isNothing, on,
                                                                (==.), (||.) )

import           Handler.SuperUser.Notice.Paramining.UserList ( getUsersWithParaminingDB )
import           Local.Persist.Currency
import           Local.Persist.Exchange
import           Local.Persist.UserRole
import           Local.Persist.Wallet
import           Type.Wallet                                  ( WalletData (..) )
import           Utils.Database.User.Wallet
import           Utils.Money
import           Yesod.WebSockets

import           Data.Aeson                                   ( (.!=), (.:),
                                                                (.:!), (.:?) )
import qualified Data.Aeson                                   as A
import           Database.Esqueleto
import           Text.Blaze.Renderer.Text                     ( renderMarkup )
import           Text.Hamlet                                  ( shamletFile )

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
            "orders stats" -> do
                (activeOrders, aoStats) <- liftHandler getActiveOrdersStats
                let aoAmountStats = map (\(x, y, _) -> (pair2codes x, y)) aoStats
                let aoLeftStats = map (\(x, _, z) -> (pair2codes x, z)) aoStats
                (executions, exeStats, exeFeeStats) <- liftHandler getOrdersExecutionStats
                let exeTransStats = map (\(x, y, _) -> (pair2codes x, y)) exeStats
                let exeAmountStats = map (\(x, _, z) -> (pair2codes x, z)) exeStats
                send' $ countEventToJson "Orders Active Count" activeOrders
                send' $ countsEventToJson "Orders Active Amount Stats" aoAmountStats
                send' $ countsEventToJson "Orders Active Left Stats" aoLeftStats
                send' $ countEventToJson "Order Executions Count" executions
                send' $ countsEventToJson "Order Executions Transfer Stats" exeTransStats
                send' $ countsEventToJson "Order Executions Amount Stats" exeAmountStats
                send' $ countsEventToJson "Order Executions Fee Stats" (pairMapCurrencyCode exeFeeStats)
            mayJsonCommand -> case parseJSONCommand mayJsonCommand of
                Just cmd -> execSUCommand cmd
                Nothing  -> sendTextData $ "Command not recognized: " ++ t
  where
    send' = sendTextData . decodeUtf8 . A.encode
    pair2codes p =
        let (c1, c2) = unPairCurrency p
        in currencyCodeT c1 <> " " <> currencyCodeT c2

    parseJSONCommand :: Text -> Maybe SUCommand
    parseJSONCommand = A.decode . encodeUtf8 . fromStrict

execSUCommand :: SUCommand -> WebSocketsT Handler ()
execSUCommand cmd@(SUCmdWalletParaminingData wt inc) = do
    walletByToken <- liftHandler . runDB $ getBy (UniqueWalletToken wt)
    res <- case walletByToken of
        Nothing -> pure A.Null
        Just w@(Entity _ wallet) -> liftHandler $ do
            now <- liftIO getCurrentTime
            fmap toJSON <$> runDB $ do
                paraTime <- lastWalletStrictParaTimeDB w
                os       <- getUserWalletActiveOrders w
                rs       <- getUserWalletActiveWithdrawal w
                let stats = foldUserWalletStats w os rs paraTime
                    walCents = userWalletAmountCents wallet
                    wc = userWalletCurrency wallet
                    ordCents = walletDataOrdersCents stats
                    wreqCents = walletDataWithdrawalCents stats
                    cents = walCents + ordCents + wreqCents
                    paramining = paraTime >>= \t ->
                        if cents < 1
                        then
                            Just (0, 0, t)
                        else
                            (\(v, k) -> (v, k, t)) <$>
                                currencyAmountPara now t wc cents
                return $ object
                    [ "stats" .= toJSON stats
                    , "paramining" .= case paramining of
                        Nothing -> A.Null
                        Just (paraCents, _, _) -> object
                            [ "amount" .= toJSON paraCents ]
                    ]
    wsSendJSON (SUCmdResponseWalletParaminingData res)

execSUCommand cmd@(SUCmdWalletParaminingDataPage l o inc) = do
    (more, pageData) <- liftHandler . runDB $ getUsersWithParaminingDB l o
    page <- toJSON <$> if inc
        then includeHTML pageData
        else pdToJSON pageData
    wsSendJSON (SUCmdResponseWalletParaminingDataPage more page)
  where
    pdToJSON pageData = do
        let res = flip map pageData $ \(u, w, _, p) -> object
                [ "user" .= toJSON u
                , "wallet" .= toJSON w
                , "paramining-monthly-rate" .= toJSON p
                ]
        return res

    includeHTML pageData = do
        htmlRenders <- liftHandler $ mapM render pageData
        let zips =  zip pageData htmlRenders
        return $ flip map zips $ \((u, w, c, p), h) -> object
            [ "user" .= toJSON u
            , "wallet" .= toJSON w
            , "paramining-monthly-rate" .= toJSON p
            , "html" .= (toJSON . renderMarkup) h
            ]

    render (Entity uid u, Entity wid w, c, p) = do
        renderAmount <- getAmountRenderer
        return
            $(shamletFile "templates/su/notice/paramining/wallet-item.hamlet")


data SUCommand
    = SUCmdWalletParaminingDataPage Int UserWalletId Bool
    | SUCmdWalletParaminingData Text Bool

instance ToJSON SUCommand where
    toJSON cmd@(SUCmdWalletParaminingDataPage lim ofs incHtml) = object $
        [ "per-page" .= toJSON lim
        , "offset-id" .= fromSqlKey ofs
        , "include-html" .= toJSON incHtml ]
        <> defaultSUCommandJSONProps (suCmdName cmd)

    toJSON cmd@(SUCmdWalletParaminingData tok incHtml) = object $
        [ "wallet" .= toJSON (tok :: Text)
        , "include-html" .= toJSON incHtml ]
        <> defaultSUCommandJSONProps (suCmdName cmd)

instance FromJSON SUCommand where
    parseJSON (A.Object o) = do
        typ <- o .: "type"
        name <- o .: "name"
        includeHTML <- o .:! "include-html"  .!= False
        if typ == "command"
            then case name of
                "wallet-paramining-data-page" -> SUCmdWalletParaminingDataPage
                    <$> o .: "per-page"
                    <*> (toSqlKey <$> o .: "offset-id")
                    <*> pure includeHTML
                "wallet-paramining-data" -> SUCmdWalletParaminingData
                    <$> o .: "wallet"
                    <*> pure includeHTML
                otherName -> fail $
                     "Unknown super user command name " ++ otherName
            else fail $ "Unknown super user command type " ++ typ
    parseJSON _ = fail "Expected a command object"


defaultSUCommandJSONProps :: Text -> [(Text, A.Value)]
defaultSUCommandJSONProps name =
    [ "type" .= toJSON ("command" :: Text)
    , "name" .= toJSON name ]

data SUCommandResponse
    = SUCmdResponseWalletParaminingDataPage Bool A.Value
    | SUCmdResponseWalletParaminingData A.Value

instance ToJSON SUCommandResponse where
    toJSON r@(SUCmdResponseWalletParaminingDataPage more list) = object
        [ "type" .= ("response" :: Text)
        , "command" .= suResponseCmdName r
        , "next-page" .= more
        , "data" .= list ]
    toJSON r@(SUCmdResponseWalletParaminingData list) = object
        [ "type" .= ("response" :: Text)
        , "command" .= suResponseCmdName r
        , "data" .= list ]


suCmdName :: SUCommand -> Text
suCmdName (SUCmdWalletParaminingDataPage _ _ _) = "wallet-paramining-data-page"
suCmdName (SUCmdWalletParaminingData _ _) = "wallet-paramining-data"


suResponseCmdName :: SUCommandResponse -> Text
suResponseCmdName (SUCmdResponseWalletParaminingDataPage _ _) =
    "wallet-paramining-data-page"
suResponseCmdName (SUCmdResponseWalletParaminingData _) =
    "wallet-paramining-data"

data CountEvent = CountEvent
  { ceObjectType :: Text
  , ceValue      :: Int }

countEventToJson :: Text -> Int -> A.Value
countEventToJson typ n = A.object
  [ "type" A..= ("count-event" :: Text)
  , "object" A..= typ
  , "value" A..= n ]

countsEventToJson :: Text -> [(Text, Int)] -> A.Value
countsEventToJson typ vs = A.object
  [ "type" A..= ("count-event" :: Text)
  , "object" A..= typ
  , "value" A..= object (map (uncurry (A..=)) vs) ]


getUsersCount :: Handler Int
getUsersCount =
    fmap take1st <$> runDB $ select $ from (\u -> do
        where_ (u ^. UserRole ==. val Client)
        return $ count (u ^. UserId))


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
    stats <- fmap unValues3 <$> select $ from $
        \(r `InnerJoin` w) -> do
            on (r ^. WithdrawalRequestWalletId ==. w ^. UserWalletId)
            where_ (isNothing $ r ^. WithdrawalRequestAccepted)
            let c = w ^. UserWalletCurrency
            groupBy c
            let sa = sum_ (r ^. WithdrawalRequestCentsAmount)
            let sf = sum_ (r ^. WithdrawalRequestFrozenAmount)
            return (c, sa, sf)
    return (cnt, stats)

getAcceptedWithdrawalStats :: Handler (Int, [ (Currency, Int, Int) ])
getAcceptedWithdrawalStats = runDB $ do
    cnt <- fmap take1st <$> (select . from) $ \(r `InnerJoin` e) -> do
        on (r ^. WithdrawalRequestId ==. e ^. WithdrawalAcceptRequestId)
        return (count $ r ^. WithdrawalRequestId)
    stats <- fmap unValues3 <$> select $ from $
        \(w `InnerJoin` r `InnerJoin` e) -> do
            on (r ^. WithdrawalRequestId ==. e ^. WithdrawalAcceptRequestId)
            on (r ^. WithdrawalRequestWalletId ==. w ^. UserWalletId)
            let c = w ^. UserWalletCurrency
            groupBy c
            let st = sum_ (e ^. WithdrawalAcceptAmountTransfered)
            let sf = sum_ (e ^. WithdrawalAcceptActualFee)
            return (c, st, sf)
    return (cnt, stats)


{- WALLETS -}

getWalletBalances :: Handler [ (Currency, Maybe Rational) ]
getWalletBalances = fmapUnValuePair
    <$> runDB $ select $ from (\w -> do
            let c = w ^. UserWalletCurrency
            groupBy c
            return (c, sum_ (w ^. UserWalletAmountCents)))


{- ORDERS -}

getActiveOrdersStats :: Handler (Int, [(ExchangePair, Int, Int)])
getActiveOrdersStats = runDB $ do
    cnt <- fmap take1st <$> select . from $ \o -> do
        where_ $ o ^. ExchangeOrderIsActive ==. val True
        return countRows
    stats <- fmap unValues3 <$> (select . from) $ \ o -> do
        where_ $ o ^. ExchangeOrderIsActive ==. val True
        groupBy $ o ^. ExchangeOrderPair
        let sa = sum_ $ o ^. ExchangeOrderAmountCents
        let sl = sum_ $ o ^. ExchangeOrderAmountLeft
        return (o ^. ExchangeOrderPair, sa, sl)
    return (cnt, stats)

getOrdersExecutionStats :: Handler (Int, [(ExchangePair, Int, Int)], [(Currency, Int)])
getOrdersExecutionStats = runDB $ do
    cnt <- fmap take1st <$> select . from $ \(e, o) -> do
        where_ (e ^. ExchangeOrderExecutionOrderId ==. o ^. ExchangeOrderId)
        groupBy $ o ^. ExchangeOrderPair
        return countRows
    trans <- fmap unValues3 <$> (select . from) $ \ (e, o) -> do
        where_ (e ^. ExchangeOrderExecutionOrderId ==. o ^. ExchangeOrderId)
        groupBy $ o ^. ExchangeOrderPair
        let st = sum_ $ e ^. ExchangeOrderExecutionTransferAmountCents
        let sa = sum_ $ e ^. ExchangeOrderExecutionIncomeAmountCents
        return (o ^. ExchangeOrderPair, st, sa)
    fee <- fmap (pair2currencyFst . unValues2) <$> (select . from) $ \ (e, o) -> do
            where_ (e ^. ExchangeOrderExecutionOrderId ==. o ^. ExchangeOrderId)
            groupBy $ o ^. ExchangeOrderPair
            let s = sum_ $ e ^. ExchangeOrderExecutionFeeCents
            return (o ^. ExchangeOrderPair, s)
    return (cnt, trans, fee)
  where
    pair2currencyFst :: [(ExchangePair, a)] -> [(Currency, a)]
    pair2currencyFst = map (\(p, i) -> ((fst . unPairCurrency) p, i))


{- UTILS -}

wsSendJSON :: SUCommandResponse -> WebSocketsT Handler ()
wsSendJSON = sendTextData . decodeUtf8 . A.encode

take1st :: Num p => [Database.Esqueleto.Value p] -> p
take1st []    = 0
take1st (x:_) = unValue x

fmapUnValuePair
    :: Handler [(Value a, Value b)]
    -> Handler [(a, b)]
fmapUnValuePair = fmap (map (\(a, b) -> (unValue a, unValue b)))

fmapUnValueTriple
    :: Handler [(Value a, Value b, Value c)]
    -> Handler [(a, b, c)]
fmapUnValueTriple = fmap (map (\(a, b, c) -> (unValue a, unValue b, unValue c)))

unValues2
        :: [(Value a, Value (Maybe Rational))]
        -> [(a, Int)]
unValues2 = map
    (\(x, y) -> (unValue x, (may0 . unValue) y))

unValues3
        :: [(Value a, Value (Maybe Rational), Value (Maybe Rational))]
        -> [(a, Int, Int)]
unValues3 = map
    (\(x, y, z) -> (unValue x, (may0 . unValue) y, (may0 . unValue) z))

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

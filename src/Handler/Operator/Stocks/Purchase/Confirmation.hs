{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Operator.Stocks.Purchase.Confirmation where

import           Import                             hiding ( on, (==.) )
import qualified Import                             as I

import           Handler.API.Render.Stocks.ListItem ( renderClientStocksPurchase )
import           Local.Persist.Currency             ( Currency, pzmC )
import           Local.Persist.ReferralBounty       ( ReferralBountyType (RegistrationBounty) )
import           Local.Persist.Wallet
import           Type.Money                         ( oneCoinCents )

import           Database.Esqueleto                 hiding ( (-=.), (=.) )
import           Text.Blaze.Html.Renderer.Text      ( renderHtml )


postOperatorStocksPurchaseConfirmationR ::
    StocksPurchaseId -> Handler TypedContent
postOperatorStocksPurchaseConfirmationR pid = do
    staff    <- requireOperatorId
    purchase <- runDB $ queryPurchaseDetails pid
    render   <- getMessageRender
    case purchase of
        [] -> selectRep $ do
            let message = MsgAPIInvalidStocksPurchaseToken
            redirectWithMessages     [("form", message)] [ ] HomeR
            respondJSONErrorMessages (render message)    [ ]
        (Entity _ p, Entity _ s, Entity _ a) : _ -> do
            timeNow <- liftIO getCurrentTime
            (staffId, staffIdent) <- case staff of
                Right su -> pure (Nothing, Just su)
                Left uid -> do
                    op <- userIdent <$> (runDB $ getJust uid)
                    return (Just uid, Just op)
            let stocks = stocksPurchaseStocks p
                amount = stocksPurchaseAmount p
            purchase' <- runDB $ do
                updateWhere
                    [ StocksActiveStock I.==. stocks ]
                    [ StocksActiveLeft -=. amount ]
                updateGet
                    pid
                    [ StocksPurchaseAccepted =. Just timeNow
                    , StocksPurchaseAcceptedBy =. staffId
                    , StocksPurchaseAcceptedByIdent =. staffIdent ]
            _ <- accrueReferralBounties (Entity pid p)
            notifyPublic s a amount
            notifyClient purchase' s amount
            redirect OperatorStocksPurchaseIndexR

notifyPublic :: Stocks -> StocksActive -> Int -> Handler ()
notifyPublic s a n = do
    ch <- appChannelsPublicNotifications . appChannels <$> getYesod
    let notice = object
            [ "type"        .= ("stocks-availability-change" :: Text)
            , "stocks"      .= stocksAbbr s
            , "amount-left" .= (stocksActiveLeft a - n)
            ]
    liftIO . atomically $ writeTChan ch notice

notifyClient :: StocksPurchase -> Stocks -> Int -> Handler ()
notifyClient p s n = do
    let u   = stocksPurchaseUser p
        sid = stocksPurchaseStocks p
    ch <- appChannelsClientNotifications . appChannels <$> getYesod
    html <- renderClientStocksPurchase p s
    let notice = object
            [ "type"        .= ("client-stocks-purchase-details" :: Text)
            , "client"      .= fromSqlKey u
            , "pack-id"     .= fromSqlKey sid
            , "pack-name"   .= stocksName s
            , "pack-abbr"   .= stocksAbbr s
            , "pack-price"  .= stocksPrice s
            , "event"       .= ("purchase-confirmation" :: Text)
            , "amount"      .= n
            , "purchase-id" .= stocksPurchaseToken p
            , "time"        .= stocksPurchaseAccepted p
            , "html-render" .= renderHtml html
            ]
    liftIO . atomically $ writeTChan ch (u, notice)

accrueReferralBounties ::
    Entity StocksPurchase -> Handler [Entity ReferralBounty]
accrueReferralBounties (Entity _ p) = do
    let owner' = stocksPurchaseUser p
    bounty' <- runDB $ queryUserRegistrationBounty owner'
    case bounty' of
        Nothing -> return [] -- No such user
        Just bounty -> case bounty of
            (owner, Nothing) -> do
                bounties <- collectBounties owner
                runDB $ mapM insertBounties bounties
            (_, Just _)      -> return [] -- bounties already given
  where
    insertBounties (_, reason, mkBounty, mkTransaction)  = do
        rid <- insert reason
        let bounty      = mkBounty rid
        let transaction = mkTransaction rid
        let wid         = walletBalanceTransactionWalletId transaction
        let before      = walletBalanceTransactionAmountCentsBefore transaction
        let amount      = transactionAmount transaction
        let time        = walletBalanceTransactionTime transaction
        let balance     = before + amount
        _  <- insert transaction
        _ <- I.update wid [ UserWalletAmountCents =. balance
                          , UserWalletLastParaTime =. Just time ]
        insertEntity bounty

    transactionAmount t = case walletBalanceTransactionType t of
            ReferralBounty' n -> n
            _                 -> 0

collectBounties ::
       Entity User
    -> Handler [( Entity User
                , WalletTransactionReason
                , WalletTransactionReasonId -> ReferralBounty
                , WalletTransactionReasonId -> WalletBalanceTransaction )]
collectBounties u = do
    timeNow <- liftIO getCurrentTime
    let (Entity uid _) = u
    (Entity wid w) <- getOrCreateWallet uid pzmC
    let reason   = WalletTransactionReason wid
    let amount   = 10 * oneCoinCents
    let mkBounty =
            registrationBountyWithReason 0 uid uid amount pzmC
    let centsBefore   = userWalletAmountCents w
    let mkTransaction =
            bountyTransactionWithReason wid centsBefore amount timeNow
    maxLevel          <- appRefMaxLevels  . appSettings <$> getYesod
    referrersBounties <- collectReferrerBounties uid maxLevel
    return $ (u, reason, mkBounty, mkTransaction) : referrersBounties

collectReferrerBounties ::
       UserId
    -> Int
    -> Handler [( Entity User
                , WalletTransactionReason
                , WalletTransactionReasonId -> ReferralBounty
                , WalletTransactionReasonId -> WalletBalanceTransaction )]
collectReferrerBounties uid maxLevel
    | maxLevel > 0 = do
        let lvl = 1
        let n = maxLevel
        collect uid lvl n
    | otherwise = pure []
  where
    collect referral lvl n = do
        mayRef <- getReferrerWallet referral
        case mayRef of
            Nothing -> return []
            Just (ref, w) -> do
                let (Entity rid _) = ref
                timeNow <- liftIO getCurrentTime
                let (r, b, t) =  mk timeNow w rid referral lvl n
                rest <- if lvl < maxLevel
                        then collect rid (lvl + 1) (n - 1)
                        else pure []
                return $ (ref, r, b, t) : rest

    mk t (Entity wid w) ref usr l n =
        let before        = userWalletAmountCents w
            reason        = WalletTransactionReason wid
            amount        = n * oneCoinCents
            mkBounty      = registrationBountyWithReason l ref usr amount pzmC
            mkTransaction = bountyTransactionWithReason wid before amount t
        in (reason, mkBounty, mkTransaction)

getReferrerWallet :: UserId -> Handler (Maybe (Entity User, Entity UserWallet))
getReferrerWallet uid = do
    vals <- runDB $ queryReferrerWallet uid
    case vals of
        [] -> return Nothing
        (r, mayWallet) : _ -> do
            realWallet <- maybe (runDB (createPzmWallet r)) return mayWallet
            return $ Just (r, realWallet)
  where
    createPzmWallet (Entity u _) = do
        t <- liftHandler appNonce128urlT
        now <- liftIO getCurrentTime
        let w = UserWallet u pzmC 0 t now Nothing
        (flip Entity w) <$> insert w


queryPurchaseDetails ::
       MonadIO m
    => StocksPurchaseId
    -> SqlPersistT
            m
            [(Entity StocksPurchase, Entity Stocks, Entity StocksActive)]
queryPurchaseDetails pid = select . from $
    \(p `InnerJoin` s `InnerJoin` a) -> do
        on (a ^. StocksActiveStock ==. s ^. StocksId)
        on (s ^. StocksId ==. p ^. StocksPurchaseStocks)
        where_ (p ^. StocksPurchaseId ==. val pid)
        limit 1
        return (p, s, a)

queryUserRegistrationBounty :: MonadIO m =>
    UserId -> SqlPersistT m (Maybe ( Entity User
                                   , Maybe (Entity ReferralBounty) ))
queryUserRegistrationBounty uid = (listToMaybe <$>) <$> select . from $
    \(u `LeftOuterJoin` rb) -> do
        on
            (   (rb ?. ReferralBountyReferral ==. just (u ^. UserId))
            &&. (rb ?. ReferralBountyType ==. just (val RegistrationBounty))
            &&. (rb ?. ReferralBountyLevel ==. just (val 0)) )
        where_ $ u ^. UserId ==. val uid
        limit 1
        return (u, rb)


queryReferrerWallet :: MonadIO m =>
    UserId -> SqlPersistT m  [(Entity User, Maybe (Entity UserWallet))]
queryReferrerWallet uid = select . from $
    \(u `InnerJoin` r `InnerJoin` referrer `InnerJoin` refUser `LeftOuterJoin` w) -> do
        on (w ?. UserWalletUserId ==. just (refUser ^. UserId))
        on (refUser ^. UserId ==. referrer ^. ReferrerUser)
        on (referrer ^. ReferrerId ==. r ^. ReferralReferrer)
        on (r ^. ReferralUser ==. u ^. UserId)
        where_
            ( (u ^. UserId ==. val uid)
            &&. (w ?. UserWalletCurrency ==. just (val pzmC)) )
        limit 1
        return (refUser, w)

registrationBountyWithReason ::
       Int
    -> UserId
    -> UserId
    -> Int
    -> Currency
    -> WalletTransactionReasonId
    -> ReferralBounty
registrationBountyWithReason l ref rel a c =
    ReferralBounty RegistrationBounty l ref rel a c

bountyTransactionWithReason ::
       UserWalletId
    -> Int
    -> Int
    -> UTCTime
    -> WalletTransactionReasonId
    -> WalletBalanceTransaction
bountyTransactionWithReason wid c a t rid =
    WalletBalanceTransaction wid (ReferralBounty' a) rid c t ReferralBountyAccrual

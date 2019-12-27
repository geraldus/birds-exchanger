{-# LANGUAGE OverloadedStrings #-}
module Handler.Operator.Stocks.Purchase.Confirmation where

import           Import             hiding ( on, (==.) )
import qualified Import             as I

import           Database.Esqueleto hiding ( (-=.), (=.) )


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
            ]
    liftIO . atomically $ writeTChan ch (u, notice)


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



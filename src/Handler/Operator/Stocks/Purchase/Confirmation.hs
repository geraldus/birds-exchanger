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
            runDB $ do
                _ <- updateGet
                    pid
                    [ StocksPurchaseAccepted =. Just timeNow
                    , StocksPurchaseAcceptedBy =. staffId
                    , StocksPurchaseAcceptedByIdent =. staffIdent ]
                updateWhere
                    [ StocksActiveStock I.==. stocks ]
                    [ StocksActiveLeft -=. amount ]
            notifyPublic s a amount
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



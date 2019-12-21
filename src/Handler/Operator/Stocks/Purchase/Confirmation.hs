{-# LANGUAGE OverloadedStrings #-}
module Handler.Operator.Stocks.Purchase.Confirmation where

import Import


postOperatorStocksPurchaseConfirmationR ::
    StocksPurchaseId -> Handler TypedContent
postOperatorStocksPurchaseConfirmationR pid = do
    staff    <- requireOperatorId
    purchase <- runDB $ get pid
    render   <- getMessageRender
    case purchase of
        Nothing -> selectRep $ do
            let message = MsgAPIInvalidStocksPurchaseToken
            redirectWithMessages     [("form", message)] [ ] HomeR
            respondJSONErrorMessages (render message)    [ ]
        Just p -> do
            timeNow <- liftIO getCurrentTime
            (staffId, staffIdent) <- case staff of
                Right su -> pure (Nothing, Just su)
                Left uid -> do
                    op <- userIdent <$> (runDB $ getJust uid)
                    return (Just uid, Just op)
            let stocks = stocksPurchaseStocks p
                amount = stocksPurchaseAmount p
            runDB $ do
                update
                    pid
                    [ StocksPurchaseAccepted =. Just timeNow
                    , StocksPurchaseAcceptedBy =. staffId
                    , StocksPurchaseAcceptedByIdent =. staffIdent ]
                updateWhere
                    [ StocksActiveStock ==. stocks ]
                    [ StocksActiveLeft -=. amount ]
            redirect OperatorStocksPurchaseIndexR
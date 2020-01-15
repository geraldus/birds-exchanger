{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module Handler.Operator.Stocks.Purchase.Details where

import           Import                                 hiding ( on, (==.) )

import           Handler.Operator.Stocks.Purchase.Index ( FullPurchaseDetails,
                                                          purchaseDataJoins,
                                                          purchaseItemW )

import           Database.Esqueleto                     as E


getOperatorStocksPurchaseDetailsR :: StocksPurchaseId -> Handler Html
getOperatorStocksPurchaseDetailsR pid = do
    _ <- requireOperatorId
    renderUrl <- getUrlRender
    render <- getMessageRender
    d <- runDB $ queryPurchaseFullDetails pid
    let body = case d of
            [] -> do
                addMessageI "invalid-url" MsgAPIInvalidStocksPurchaseToken
                redirect OperatorStocksPurchaseIndexR
            d' : _ -> purchaseItemW d'
    let linkText = render MsgPageTitleStocksPending
        url = renderUrl OperatorStocksPurchaseIndexR
    defaultLayout $ do
        $(widgetFile "operator/common")
        $(widgetFile "operator/stocks/purchase/index")

queryPurchaseFullDetails::
    MonadIO m => StocksPurchaseId -> SqlPersistT m [FullPurchaseDetails]
queryPurchaseFullDetails pid = select . from $
    \q@(e `InnerJoin` u `InnerJoin` p `InnerJoin` s `InnerJoin` a) -> do
        purchaseDataJoins q
        orderBy
            [ asc (p ^. StocksPurchaseId) ]
        where_ (p ^. StocksPurchaseId ==. val pid)
        limit 1
        return (p, s, a, u, e)

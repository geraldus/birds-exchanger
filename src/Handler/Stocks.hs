{-# LANGUAGE OverloadedStrings #-}
module Handler.Stocks ( getStocksR ) where

import           Import                 hiding ( on, (==.) )

import           Local.Persist.Currency ( pzmC )
import           Stocks.Widgets         ( purchaseStatusW )
import           Utils.App.Client       ( dateTimeRowW )
import           Utils.Stocks           ( purchaseSignificantDate )

import           Database.Esqueleto
import           Text.Julius            ( rawJS )


getStocksR :: Handler Html
getStocksR = defaultLayout $ do
    setAppPageTitle MsgStocks
    twoColsLayout
        noCSRF noPredefinedId MsgStocks title leftCol rightCol
    client <- handlerToWidget maybeClientId
    clientHistoryW client
  where
    title = [whamlet|_{MsgPageTitleStocks}|]

    leftCol  = do
        formId <- newIdent
        colHeaderMessage MsgStocksSubtitleBuy
        buyForm (Just formId) Nothing

    rightCol = do
        colHeaderMessage MsgStocksSubtitleSell
        sellForm Nothing Nothing

    noPredefinedId = Nothing

    noCSRF = mempty

    colHeaderMessage hm =
        [whamlet|<h2 .h5 .col-header .text-uppercase>_{hm}|]

    clientHistoryW Nothing = mempty
    clientHistoryW (Just client) = do
        list <- handlerToWidget . runDB $ queryClientPurchases client
        let body = mapM_ clientHistoryItemW list
        $(widgetFile "client/stocks/index")

    clientHistoryItemW (Entity _ p, Entity _ s) = do
        render <- handlerToWidget getUrlRender
        htmlId <- newIdent
        let purchaseId = stocksPurchaseToken p
            stocksAmount = stocksPurchaseAmount p
            stocksPackName = stocksName s
            significantDate = dateTimeRowW (purchaseSignificantDate p)
            purchaseStatus = purchaseStatusW htmlId p s
            url = render (ClientStocksPurchaseDetailsR purchaseId)
        $(widgetFile "client/stocks/list-item")


buyForm :: Maybe Text -> Maybe Text -> Widget
buyForm idMaybe classMaybe = do
    let formClass = fromMaybe "stocks-buy-form .mt-5 .mt-lg-0" classMaybe
    formId <- maybe newIdent return idMaybe
    maybeClientUser <- handlerToWidget maybeClientAuthPair
    $(widgetFile "form/stocks/buy")
    $(widgetFile "messages/fenix-stocks/pack-desc/generic")
    $(widgetFile "messages/fenix-stocks/pack-desc/start")
    $(widgetFile "messages/fenix-stocks/pack-desc/standard")
    $(widgetFile "messages/fenix-stocks/pack-desc/premium")

sellForm :: Maybe Text -> Maybe Text -> Widget
sellForm idMaybe classMaybe = do
    formId <- maybe newIdent return idMaybe
    let formClass = fromMaybe "stocks-sell-form" classMaybe
    $(widgetFile "form/stocks/sell")
    $(widgetFile "messages/sell-not-avail-yet")

queryClientPurchases ::
       MonadIO m
    => UserId -> SqlPersistT m [(Entity StocksPurchase, Entity Stocks)]
queryClientPurchases uid = select . from $
    \(u `InnerJoin` p `InnerJoin` s) -> do
        on (s ^. StocksId ==. p ^. StocksPurchaseStocks)
        on (p ^. StocksPurchaseUser ==. u ^. UserId)
        orderBy [ desc (p ^. StocksPurchaseCreated) ]
        where_ (u ^. UserId ==. val uid)
        return (p, s)

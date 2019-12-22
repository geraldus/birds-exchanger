{-# LANGUAGE OverloadedStrings #-}
module Handler.Stocks ( getStocksR ) where

import           Import                     hiding ( on, (==.) )

import           Local.Persist.Currency     ( pzmC )
import           Stocks.Widgets             ( purchaseStatusW )
import           Utils.App.Client           ( dateTimeRowW )
import           Utils.Database.Stocks      ( queryStocksActives )
import           Utils.Database.User.Stocks ( queryClientPurchases )
import           Utils.Stocks               ( purchaseSignificantDate )

import           Text.Julius                ( rawJS )


getStocksR :: Handler Html
getStocksR =
    defaultLayout $ do
        setAppPageTitle MsgStocks
        twoColsLayout
            noCSRF noPredefinedId MsgStocks title leftCol rightCol
        client <- handlerToWidget maybeClientId
        clientHistoryW client
  where
    title = [whamlet|_{MsgPageTitleStocks}|]

    leftCol  = do
        formId <- newIdent
        stocksActives <- liftHandler . runDB $ queryStocksActives
        colHeaderMessage MsgStocksSubtitleBuy
        buyForm stocksActives (Just formId) Nothing

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


buyForm ::
       [(Entity StocksActive, Entity Stocks)]
    -> Maybe Text
    -> Maybe Text
    -> Widget
buyForm actives idMaybe classMaybe = do
    let formClass = fromMaybe "stocks-buy-form .mt-5 .mt-lg-0" classMaybe
    formId <- maybe newIdent return idMaybe
    maybeClientUser <- handlerToWidget maybeClientAuthPair
    let fnxBLeft = findActive "FNXB" actives
    let fnxSLeft = findActive "FNXS" actives
    let fnxPLeft = findActive "FNXP" actives
    $(widgetFile "form/stocks/buy")
    $(widgetFile "messages/fenix-stocks/pack-desc/generic")
    $(widgetFile "messages/fenix-stocks/pack-desc/start")
    $(widgetFile "messages/fenix-stocks/pack-desc/standard")
    $(widgetFile "messages/fenix-stocks/pack-desc/premium")
  where
    findActive abr =
        maybe 0 (stocksActiveLeft . entityVal . fst)
            . stocksActiveByAbbr abr

sellForm :: Maybe Text -> Maybe Text -> Widget
sellForm idMaybe classMaybe = do
    formId <- maybe newIdent return idMaybe
    let formClass = fromMaybe "stocks-sell-form" classMaybe
    $(widgetFile "form/stocks/sell")
    $(widgetFile "messages/sell-not-avail-yet")

stocksActiveByAbbr ::
       Text
    -> [(Entity StocksActive, Entity Stocks)]
    -> Maybe (Entity StocksActive, Entity Stocks)
stocksActiveByAbbr abr = find stockAbr
    where stockAbr (_, Entity _ s) = stocksAbbr s == abr
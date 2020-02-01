{-# LANGUAGE OverloadedStrings #-}
module Handler.Stocks ( getStocksR, findStocksActive ) where

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
        singleColLayout
            noCSRF noPredefinedId MsgStocks title leftCol
        client <- handlerToWidget maybeClientId
        clientHistoryW client
  where
    title = [whamlet|_{MsgPageTitleStocks}|]

    leftCol  = do
        formId <- newIdent
        stocksActives <- liftHandler . runDB $ queryStocksActives
        colHeaderMessage MsgStocksSubtitleBuy
        buyForm stocksActives (Just formId) Nothing

    noPredefinedId = Nothing

    noCSRF = mempty

    colHeaderMessage hm =
        [whamlet|<h2 .h5 .col-header .text-uppercase>_{hm}|]

    clientHistoryW Nothing = mempty
    clientHistoryW (Just client) = do
        urlRender <- getUrlRender
        list <- handlerToWidget . runDB $ queryClientPurchases client
        let body = mapM_ clientHistoryItemW list
            clientSocketUrl = urlRender ClientNotificationsWebSocketR
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
    let formClass =
            fromMaybe
                "stocks-buy-form mt-5 mt-lg-0 col-12 col-lg-7"
                classMaybe
    formId <- maybe newIdent return idMaybe
    htmlId <- newIdent
    creds  <- handlerToWidget maybeClientCreds
    let fnxBLeft = fst $ findStocksActive "FNXB" actives
    let fnxSLeft = fst $ findStocksActive "FNXS" actives
    let fnxPLeft = fst $ findStocksActive "FNXP" actives
    let maybeClientUser  = fst <$> creds
        maybeClientEmail = snd <$> creds
        hasVerifiedEmail =
            maybe
                False
                (isNothing . emailVerkey . entityVal)
                maybeClientEmail
    let verificationGuide = do
            let email = maybe mempty (emailEmail . entityVal) maybeClientEmail
            $(widgetFile "messages/email-verification")
    $(widgetFile "form/stocks/buy")
    $(widgetFile "messages/fenix-stocks/pack-desc/generic")
    $(widgetFile "messages/fenix-stocks/pack-desc/start")
    $(widgetFile "messages/fenix-stocks/pack-desc/standard")
    $(widgetFile "messages/fenix-stocks/pack-desc/premium")


findStocksActive :: Text -> [(Entity StocksActive, Entity Stocks)] -> (Int, Int)
findStocksActive abr = maybe (0, 0) stats . stocksActiveByAbbr abr
  where stats (Entity _ s, _) = (stocksActiveLeft s, stocksActiveTotal s)


stocksActiveByAbbr ::
       Text
    -> [(Entity StocksActive, Entity Stocks)]
    -> Maybe (Entity StocksActive, Entity Stocks)
stocksActiveByAbbr abr = find stockAbr
    where stockAbr (_, Entity _ s) = stocksAbbr s == abr

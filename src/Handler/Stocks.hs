{-# LANGUAGE OverloadedStrings #-}
module Handler.Stocks ( getStocksR ) where

import           Import

import           Local.Persist.Currency ( pzmC )

import           Text.Julius            ( rawJS )



getStocksR :: Handler Html
getStocksR = defaultLayout $ do
    setAppPageTitle MsgStocks
    twoColsLayout
        noCSRF noPredefinedId MsgStocks title leftCol rightCol
  where
    title = [whamlet|_{MsgStocks}|]

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


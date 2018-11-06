{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))

import Form.Exchanger.Order
import Local.Persist.Currency
import Local.Persist.ExchangeOrder
import Utils.Deposit (oneCoinCents)


-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo :: FileInfo
    , fileDescription :: Text
    }

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    mmsg <- getMessage
    -- mayClient <- maybeClient
    (pzmRurEFWidget, pzmRurEFEnctype) <- generateFormPost formCreateExchageOrder
    (rurPzmEFWidget, rurPzmEFEnctype) <- generateFormPost formCreateExchageOrder
    let handlerName = "getHomeR" :: Text
    allComments <- runDB $ getAllComments
    orders@(pzmRurOrders, rurPzmOrders) <- getActiveOrders
    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    mmsg <- getMessage
    (pzmRurEFWidget, pzmRurEFEnctype) <- generateFormPost formCreateExchageOrder
    (rurPzmEFWidget, rurPzmEFEnctype) <- generateFormPost formCreateExchageOrder
    let handlerName = "postHomeR" :: Text
    allComments <- runDB $ getAllComments
    (pzmRurOrders, rurPzmOrders) <- getActiveOrders
    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")


commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")

getAllComments :: DB [Entity Comment]
getAllComments = selectList [] [Asc CommentId]


getActiveOrders :: Handler ([Entity ExchangeOrder], [Entity ExchangeOrder])
getActiveOrders = do
    os <- runDB $ selectList [ExchangeOrderIsActive ==. True] []
    return $ partition (isPzmRurOrder . entityVal) os
    where isPzmRurOrder = (== CryptoC PZM) . exchangeOrderCurrencyOut


renderOrderCol :: Text -> [ExchangeOrder] -> Widget
renderOrderCol title orders = [whamlet|
    <h5 .text-center>#{title}
    <table .table.table-stripped>
        <thead>
            <tr>
                <th>Цена
                <th>Кол-во
                <th>Сумма
        <tbody>
            $forall order <- orders
                <tr>
                    <td>#{show (exchangeOrderRatio order)}
                    <td>#{renderCentsAmt (exchangeOrderAmountCents order)}
                    <td>#{renderCentsAmt (truncate (exchangeOrderRatio order * (fromIntegral . exchangeOrderAmountCents) order))}
    |]
    where
        renderCentsAmt :: Int -> Html
        renderCentsAmt amt = [shamlet|#{real}.#{frac}|]
            where
                real = truncate $ fromIntegral amt / fromIntegral oneCoinCents
                frac = amt - real * oneCoinCents
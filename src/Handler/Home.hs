{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Home where

import           Import

import           Form.Exchanger.Order
import           Local.Persist.ExchangeOrder ( ExchangePair (..) )


-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo        :: FileInfo
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
    (mmsg, mayClientUser, orderCreateFormW, (pzmRurOrders, rurPzmOrders)) <- getData
    defaultLayout $ do
        setTitle "(!) обменный пункт OutBirds"
        $(widgetFile "homepage")

getData :: HandlerFor App (Maybe Html, Maybe (Key User), Widget,
        ([Entity ExchangeOrder], [Entity ExchangeOrder]))
getData = do
    mUser <- maybeClientUser
    (,,,)
        <$> getMessage
        <*> pure mUser
        <*> fmap fst (generateFormPost (createOrderForm ExchangePzmRur))
        <*> getActiveOrders mUser

maybeClientUser :: HandlerFor App (Maybe (Key User))
maybeClientUser = (entityKey . fst <$>) <$> maybeClient


getActiveOrders :: Maybe UserId -> Handler ([Entity ExchangeOrder], [Entity ExchangeOrder])
getActiveOrders mu = do
    $(logInfo) $ pack $ show mu
    let userConstraint = case mu of
            Nothing -> []
            Just _  -> [] -- [ExchangeOrderUserId !=. uid]
    os <- runDB $ selectList
        ((ExchangeOrderIsActive ==. True) : userConstraint)
        [Asc ExchangeOrderNormalizedRatio, Asc ExchangeOrderCreated]
    return $ partition (isPzmRurOrder . entityVal) os
    where isPzmRurOrder = (== ExchangePzmRur) . exchangeOrderPair


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
                    <td>#{show (exchangeOrderNormalizedRatio order)}
                    <td>#{cents2dblT (exchangeOrderAmountLeft order)}
                    <td>#{cents2dblT (convertCents (normalizeRatio (exchangeOrderPair order) (exchangeOrderRatioNoramlization order) (exchangeOrderNormalizedRatio order)) (exchangeOrderAmountLeft order))}
    |]

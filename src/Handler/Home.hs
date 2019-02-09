{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Home where

import           Import hiding (httpLbs, decodeUtf8)

import           Form.Exchanger.Order
import           Local.Persist.Currency
import           Local.Persist.ExchangeOrder ( ExchangePair (..) )
import           Utils.Deposit
import           Utils.Render
import           Utils.Withdrawal

import           Network.HTTP.Client.TLS
import           Network.HTTP.Client.Internal
import Data.Text.Lazy.Encoding (decodeUtf8)
import           Text.Julius                    ( RawJS(..) )


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
    ratioId <- newIdent
    (mmsg, mayClientUser, orderCreateFormW, (pzmRurOrders, rurPzmOrders)) <- getData ratioId
    (btcARes, cbrRes) <- runResourceT $ liftIO $ do
        manager <- newTlsManager
        btcAReq <- parseRequest "https://btc-alpha.com/exchange/PZM_USD"
        cbrReq <- parseRequest "http://cbr.ru"
        btcARes <- httpLbs btcAReq manager
        cbrRes <- httpLbs cbrReq manager
        return (rbt btcARes, rbt cbrRes)
    defaultLayout $ do
        setTitle "(!) обменный пункт OutBirds"
        $(widgetFile "homepage")
  where
    rbt = decodeUtf8 . responseBody

getData :: Text -> HandlerFor App (Maybe Html, Maybe (Key User), Widget,
        ([Entity ExchangeOrder], [Entity ExchangeOrder]))
getData ratioId = do
    mUser <- maybeClientUser
    (,,,)
        <$> getMessage
        <*> pure mUser
        <*> fmap fst (generateFormPost (createOrderForm ratioId ExchangePzmRur))
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
    <table .table .table-stripped>
        <thead>
            <tr>
                <th>Ставка
                <th>Кол-во
                <th>Сумма
        <tbody>
            $forall order <- orders
                <tr>
                    <td>#{show (exchangeOrderNormalizedRatio order)}
                    <td>#{cents2dblT (exchangeOrderAmountLeft order)}
                    <td>#{cents2dblT (convertCents (normalizeRatio (exchangeOrderPair order) (exchangeOrderRatioNormalization order) (exchangeOrderNormalizedRatio order)) (exchangeOrderAmountLeft order))}
    |]

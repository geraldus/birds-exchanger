{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Home where

import           Import

import           Form.Exchanger.Order
import           Local.Persist.Currency
import           Local.Persist.ExchangeOrder (ExchangePair (..))
import           Utils.Deposit               ( oneCoinCents )


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
    mmsg <- getMessage
    mc <- maybeClient
    mayClientUser <- (entityKey . fst <$>) <$> maybeClient
    (orderCreateFormW, _) <- generateFormPost $ createOrderForm ExchangePzmRur
    allComments <- runDB $ getAllComments
    (pzmRurOrders, rurPzmOrders) <- getActiveOrders mayClientUser
    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        setTitle "(!) обменный пункт OutBirds"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    mmsg <- getMessage
    mayClientUser <- (entityKey . fst <$>) <$> maybeClient
    (orderCreateFormW, _) <- generateFormPost $ createOrderForm ExchangePzmRur
    allComments <- runDB $ getAllComments
    (pzmRurOrders, rurPzmOrders) <- getActiveOrders mayClientUser
    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        setTitle "(!) обменный пункт OutBirds"
        $(widgetFile "homepage")


commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")

getAllComments :: DB [Entity Comment]
getAllComments = selectList [] [Asc CommentId]


getActiveOrders :: Maybe UserId -> Handler ([Entity ExchangeOrder], [Entity ExchangeOrder])
getActiveOrders mu = do
    $(logInfo) $ pack $ show mu
    let userConstraint = case mu of
            Nothing -> []
            Just uid -> [] -- [ExchangeOrder2UserId !=. uid]
    os <- runDB $ selectList
        ([ExchangeOrderIsActive ==. True] ++ userConstraint)
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
                    <td>#{cents2dblT (exchangeOrderAmountCents order)}
                    <td>#{cents2dblT (convertCents (normalizeRatio (exchangeOrderPair order) (exchangeOrderRatioNoramlization order) (exchangeOrderNormalizedRatio order)) (exchangeOrderAmountCents order))}
    |]

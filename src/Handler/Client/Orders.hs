{-# LANGUAGE QuasiQuotes #-}
module Handler.Client.Orders where

import           Import

import           Local.Persist.Currency      ( currSign )
import           Local.Persist.ExchangeOrder ( ExchangeOrderStatus (..),
                                               ExchangePair (..) )
import           Utils.Time                  ( renderDateTimeRow )

import           Data.Time.Format            ( TimeLocale (..) )
import           Database.Persist.Sql        ( fromSqlKey )
import           Formatting
import           Text.Julius                 ( rawJS )


getClientOrdersR :: Handler Html
getClientOrdersR = do
    clientId <- requireClientId
    locale <- selectLocale
    allOrders <- runDB $ selectList
        [ ExchangeOrderUserId ==. clientId ]
        [ Desc ExchangeOrderCreated ]
    pageId <- newIdent
    defaultLayout $ do
        setTitleI MsgMyOrders
        $(widgetFile "client/orders/list")

renderOrderTr :: TimeLocale -> Entity ExchangeOrder -> Html
renderOrderTr l (Entity orderId order) = [shamlet|
    <tr #order-data-#{fromSqlKey orderId} .data-row :isActive:.active :isExecuted:.executed>
        <td .text-muted .text-center>
            <small>#{renderDateTimeRow l (exchangeOrderCreated order)}
        <td .text-center>
            <big>
                #{renderOrderExchange order}
        <td .text-center>
            <small .text-muted>x#
            \#{renderOrderRate order}&nbsp;#
            <small .text-muted>
                #{renderOrderNRatioSign order}
        <td .text-center>
            #{renderOrderRemainderExecuted l order}
        <td>
    |]
  where
    isActive = exchangeOrderIsActive order
    isExecuted = case exchangeOrderStatus order of
        Executed _ -> True
        _          -> False


renderOrderExchange :: ExchangeOrder -> Html
renderOrderExchange order = [shamlet|
    #{cents2dblT (exchangeOrderAmountCents order)}#
    \&nbsp;#
    \#{renderPairOut (exchangeOrderPair order)}#
    \&nbsp;⇢&nbsp;#
    \#{renderPairIn (exchangeOrderPair order)}#
    |]

renderOrderRate :: ExchangeOrder -> Html
renderOrderRate order = [shamlet|
    #{format (fixed 2) rate}|]
  where
    rate = exchangeOrderNormalizedRatio order

renderPairOut :: ExchangePair -> Html
renderPairOut = toHtml . currSign . fst . unPairCurrency

renderPairIn :: ExchangePair -> Html
renderPairIn = toHtml . currSign . snd . unPairCurrency

renderOrderNRatioSign :: ExchangeOrder -> Html
renderOrderNRatioSign order = [shamlet|
    #{renderPairOut rn}/#{renderPairIn rn}
    |]
  where
    rn = exchangeOrderRatioNormalization order

renderOrderRemainderExecuted :: TimeLocale -> ExchangeOrder -> Html
renderOrderRemainderExecuted l order =
    case exchangeOrderStatus order of
        Created _             -> [shamlet|<small>#{renderStatusNew}|]
        Executed t            -> [shamlet|<small>#{renderStatusExecuted t}|]
        PartiallyExecuted t e -> [shamlet|<small>#{renderStatusPartial t e}|]
        _                     -> [shamlet|>|]
  where
    renderStatusNew = [shamlet| Новый ордер, не исполнялся |]
    renderStatusExecuted t = [shamlet|
        <span>Полностью исполнен
        <br>
        <small .text-muted>
            #{renderDateTimeRow l t}
        |]
    renderStatusPartial t e = [shamlet|
        #{cents2dblT left}&nbsp;<small>#{renderPairOut pair}</small> / #
        #{cents2dblT e}&nbsp;<small>#{renderPairOut pair}</small>
        <br>
        <small .text-muted>
            #{renderDateTimeRow l t}
        |]
    left = exchangeOrderAmountLeft order
    pair = exchangeOrderPair order

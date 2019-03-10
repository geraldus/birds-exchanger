{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Client.Orders where

import           Import

import           Local.Persist.Currency ( currSign )
import           Local.Persist.Exchange ( ExchangeOrderStatus (..),
                                          ExchangePair (..) )
import           Local.Persist.Wallet   ( WalletTransactionType (..) )
import           Utils.Common           ( selectLocale )
import           Utils.Money
import           Utils.Time

import           Data.Time.Format       ( TimeLocale (..) )
import           Database.Persist.Sql   ( fromSqlKey, toSqlKey )
import           Formatting
import           Text.Julius            ( rawJS )


getClientOrdersR :: Handler Html
getClientOrdersR = do
    clientId <- requireClientId
    locale <- selectLocale
    tzo <- timezoneOffsetFromCookie
    allOrders <- runDB $ selectList
        [ ExchangeOrderUserId ==. clientId ]
        [ Desc ExchangeOrderCreated ]
    pageId <- newIdent
    msgRender <- getMessageRender
    urlRender <- getUrlRender
    defaultLayout $ do
        setAppPageTitle MsgClientOrdersPageTitle
        $(widgetFile "client/orders/list")


getClientOrderViewR :: ExchangeOrderId -> Handler Html
getClientOrderViewR orderId = do
    clientId <- requireClientId
    order <- runDB $ get404 orderId
    if exchangeOrderUserId order /= clientId
        then do
            setMessageI MsgCantViewThisOrder
            redirect ClientOrdersR
        else do
            l <- selectLocale
            tzo <- timezoneOffsetFromCookie
            let ExchangeOrder _  pair amount _alft ratioN ratio expectedFee created status isActivel _wtr = order
                r = normalizeRatio ratioN pair ratio
                expectedIn = multiplyCents r amount
            operations <- runDB $
                selectList
                    [ ExchangeOrderExecutionOrderId ==. orderId ]
                    [ Desc ExchangeOrderExecutionTime ]
            let totalIncome = totalIncomeSum operations
                totalFee = totalFeeSum operations
            defaultLayout $ do
                setTitleI MsgOrderView
                $(widgetFile "client/orders/view")
  where
    totalIncomeSum = totalSum exchangeOrderExecutionIncomeAmountCents
    totalFeeSum = totalSum exchangeOrderExecutionFeeCents
    totalSum
        :: (ExchangeOrderExecution -> Int)
        -> [Entity ExchangeOrderExecution]
        -> Int
    totalSum prop = sum . map (prop . entityVal)


renderOrderTr :: (AppMessage -> Text) -> (Route App -> Text) -> TimeLocale -> Int -> Entity ExchangeOrder -> Html
renderOrderTr messageRender urlRender l tzo (Entity orderId order) = [shamlet|
    <tr #order-data-#{fromSqlKey orderId} .data-row :isActive:.active :isExecuted:.executed>
        <td .text-muted .text-center>
            <small>#{renderDateTimeRow l tzo (exchangeOrderCreated order)}
        <td .text-center>
            <big>
                #{renderOrderExchange order}
        <td .text-center>
            <small .text-muted>x#
            \#{renderOrderRate order}&nbsp;#
            <small .text-muted>
                #{renderOrderNRatioSign order}
        <td>
            #{renderOrderRemainderExecuted l tzo order}
        <td .controls>
            <a href=#{urlRender (ClientOrderViewR orderId)}>
                <i .control .fas .fa-info-circle title="#{messageRender MsgViewOrderHistory}">
            $if isActive
                <i .order-cancel-button .control .fas .fa-times-circle title="#{messageRender MsgCancelOrder}">
                <form method=post action=#{urlRender ClientOrderCancelR}>
                    <input type=hidden name="order-id" value="#{fromSqlKey orderId}">
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

renderOrderRemainderExecuted :: TimeLocale -> Int -> ExchangeOrder -> Html
renderOrderRemainderExecuted l tzo order =
    case exchangeOrderStatus order of
        Created _             -> [shamlet|<small>#{renderStatusNew}|]
        Executed t            -> [shamlet|<small>#{renderStatusExecuted t}|]
        Cancelled t           -> [shamlet|<small>#{renderStatusCancelled t}|]
        PartiallyExecuted t e -> [shamlet|<small>#{renderStatusPartial t e}|]
        _                     -> [shamlet|>|]
  where
    renderStatusNew = [shamlet| Новый ордер, не исполнялся |]
    renderStatusExecuted t = [shamlet|
        <span>Полностью исполнен
        <br>
        <small .text-muted>
            #{renderDateTimeRow l tzo t}
        |]
    renderStatusCancelled t = [shamlet|
        <span>Отменён
        <br>
        <small .text-muted>
            #{renderDateTimeRow l tzo t}
        |]
    renderStatusPartial t e = [shamlet|
        #{cents2dblT left}&nbsp;<small>#{renderPairOut pair}</small> / #
        #{cents2dblT e}&nbsp;<small>#{renderPairOut pair}</small>
        <br>
        <small .text-muted>
            #{renderDateTimeRow l tzo t}
        |]
    left = exchangeOrderAmountLeft order
    pair = exchangeOrderPair order


orderOperationTr :: ExchangePair -> Entity ExchangeOrderExecution -> Widget
orderOperationTr pair (Entity _ op) = do
    l <- liftHandler selectLocale
    tzo <- liftHandler timezoneOffsetFromCookie
    toWidget [whamlet|
        <tr .data-row>
            <td .text-center>
                <small>#{renderDateTimeRow l tzo (exchangeOrderExecutionTime op)}
            <td>
                <small>_{MsgExchange} #
                <span>
                    <big>#{cents2dblT transfered}#
                    <small .text-muted>#{renderPairOut pair}
                $if exchangeOrderExecutionFullyExecuted op
                    <small> _{MsgOrderWasExecuted} #
            <td .text-center .align-middle>
                +#
                <span>
                    <big>#{cents2dblT (income - fee)}#
                    <small .text-muted>#{renderPairIn pair}
            <td .text-center .align-middle>
                -#
                <span>#{cents2dblT fee}
                    <small .text-muted>#{renderPairIn pair}
            |]
    where
        transfered = exchangeOrderExecutionTransferAmountCents op
        income = exchangeOrderExecutionIncomeAmountCents op
        fee = exchangeOrderExecutionFeeCents op

orderStatus :: ExchangeOrderStatus -> Widget
-- TODO: Generalize this
orderStatus (Created time) = do
    let desc = toWidget [whamlet|
            _{MsgOrderIsNew}, #
            <span .text-loweracase>_{MsgOrderWasCreated}
            |]
    orderStatus' MsgOrderIsActive desc time
orderStatus (Executed time) = do
    let desc = toWidget [whamlet|_{MsgOrderLastOperation}|]
    orderStatus' MsgOrderExecuted desc time
orderStatus (PartiallyExecuted time _) = do
    let desc = toWidget [whamlet|_{MsgOrderLastOperation}|]
    orderStatus' MsgOrderStatusExecution desc time
orderStatus (Cancelled time) = do
    let desc = toWidget [whamlet|_{MsgOrderLastOperation}|]
    orderStatus' MsgOrderCancelled desc time
orderStatus' :: AppMessage -> Widget -> UTCTime -> Widget
orderStatus' stName stDesc time = do
    l <- liftHandler selectLocale
    tzo <- liftHandler timezoneOffsetFromCookie
    toWidget [whamlet|
        <big .text-uppercase>_{stName}
        <br>
        <small .text-muted>
            ^{stDesc}
        <br>
        <small>#{renderDateTimeRow l tzo time}
        |]


postClientOrderCancelR :: Handler Html
postClientOrderCancelR = do
    orderId  <- toSqlKey <$> runInputPost (ireq intField "order-id")
    clientId <- requireClientId
    time <- liftIO getCurrentTime
    messageRender <- getMessageRender
    runDB $ do
        order <- get404 orderId
        when (exchangeOrderUserId order /= clientId) $ do
            setMessageI MsgAccessDenied
            redirect ClientOrdersR
        let currency = fst . unPairCurrency . exchangeOrderPair $ order
        (Entity walletId wallet) <- getBy404 (UniqueWallet clientId currency)
        let income = exchangeOrderAmountLeft order
            before = userWalletAmountCents wallet
        update
            orderId
            [ ExchangeOrderIsActive =. False
            , ExchangeOrderStatus =. Cancelled time ]
        reasonId <- insert $ WalletTransactionReason walletId
        insert $ WalletBalanceTransaction
            walletId (ExchangeReturn income) reasonId before time
        insert $ ExchangeOrderCancellation
            orderId
            clientId
            walletId
            reasonId
            (Just $ messageRender MsgUserCancelled)
            time
            income
        update
            walletId
            [ UserWalletAmountCents +=. income ]
    setMessageI MsgOrderWasCancelled
    redirect ClientOrdersR

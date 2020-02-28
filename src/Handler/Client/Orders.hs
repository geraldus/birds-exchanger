{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Handler.Client.Orders where

import           Import

import           Local.Persist.Currency ( currencySymbol )
import           Local.Persist.Exchange ( ExchangeOrderStatus (..),
                                          ExchangePair (..) )
import           Local.Persist.Wallet   ( TransactionTypePlain (..),
                                          WalletTransactionType (..) )
import           Utils.Common           ( selectLocale )
import           Utils.Money
import           Utils.Time

import           Data.Time.Clock        ( addUTCTime )
import           Data.Time.Format       ( TimeLocale (..) )
import           Database.Persist.Sql   ( fromSqlKey, toSqlKey )
import           Formatting
import           Text.Julius            ( rawJS )


getClientOrdersR :: Handler Html
getClientOrdersR = do
    notFound
    clientId <- requireClientId
    locale <- selectLocale
    tzo <- timezoneOffsetFromCookie
    allOrders <- runDB $ selectList
        [ ExchangeOrderUserId ==. clientId ]
        [ Desc ExchangeOrderCreated ]
    pageId <- newIdent
    msgRender <- getMessageRender
    urlRender <- getUrlRender
    let mobileList = renderMobileList allOrders locale tzo
    defaultLayout $ do
        setAppPageTitle MsgClientOrdersPageTitle
        $(widgetFile "client/orders/list")


getClientOrderViewR :: ExchangeOrderId -> Handler Html
getClientOrderViewR orderId = do
    notFound
    clientId <- requireClientId
    order <- runDB $ get404 orderId
    msgRender <- getMessageRender
    if exchangeOrderUserId order /= clientId
        then do
            setMessageI MsgCantViewThisOrder
            redirect ClientOrdersR
        else do
            l <- selectLocale
            tzo <- timezoneOffsetFromCookie
            let ExchangeOrder
                    _
                    pair
                    amount
                    alft
                    ratioN
                    ratio
                    _expectedFee
                    created
                    status
                    _isActive
                    _wtr =
                        order
                r = normalizeRatio ratioN pair ratio
                expectedIn = multiplyCents r amount
            operations <- runDB $
                selectList
                    [ ExchangeOrderExecutionOrderId ==. orderId ]
                    [ Desc ExchangeOrderExecutionTime ]
            let totalExecuted = amount - alft
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

postClientOrderCancelR :: Handler Html
postClientOrderCancelR = do
    notFound
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
        let typ = ExchangeReturn income
            typPlain = OrderCancellation
        insert $ WalletBalanceTransaction
                walletId typ reasonId before time typPlain
        insert $ ExchangeOrderCancellation
            orderId
            clientId
            walletId
            reasonId
            (Just $ messageRender MsgCancelledByUser)
            time
            income
        update
            walletId
            [ UserWalletAmountCents +=. income ]
    setMessageI MsgOrderWasCancelled
    redirect ClientOrdersR


-- | == Utils
-- | === Rendering Markup

renderOrderTr :: (AppMessage -> Text) -> (Route App -> Text) -> TimeLocale -> Int -> Entity ExchangeOrder -> Html
renderOrderTr messageRender urlRender l tzo (Entity orderId order) = [shamlet|
    <tr
        #order-data-#{fromSqlKey orderId}
        .data-row
        :isActive:.active
        :isExecuted:.executed
        >
        <td .text-muted .text-center>
            <small>#{renderDateTimeRow l tzo (exchangeOrderCreated order) []}
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
    isExecuted = isOrderExecuted order

renderOrderExchange :: ExchangeOrder -> Html
renderOrderExchange order = [shamlet|
    #{cents2dblT (exchangeOrderAmountCents order)}#
    \&nbsp;#
    \#{renderPairOut (exchangeOrderPair order)}#
    \&nbsp;⇢&nbsp;#
    \#{renderPairIn (exchangeOrderPair order)}#
    |]

renderOrderRemainderExecuted :: TimeLocale -> Int -> ExchangeOrder -> Html
renderOrderRemainderExecuted l tzo order =
    case exchangeOrderStatus order of
        Created _             -> [shamlet|<small>#{renderStatusNew}|]
        Executed t            -> [shamlet|<small>#{renderStatusExecuted t}|]
        Cancelled t           -> [shamlet|<small>#{renderStatusCancelled t}|]
        PartiallyExecuted t e -> [shamlet|<small>#{renderStatusPartial t e}|]
  where
    renderStatusNew = [shamlet| Новый ордер, не исполнялся |]
    renderStatusExecuted t = [shamlet|
        <span>Полностью исполнен
        <br>
        <small .text-muted>
            #{renderDateTimeRow l tzo t []}
        |]
    renderStatusCancelled t = [shamlet|
        <span>Отменён
        <br>
        <small .text-muted>
            #{renderDateTimeRow l tzo t []}
        |]
    renderStatusPartial t e = [shamlet|
        #{cents2dblT centsLeft}&nbsp;<small>#{renderPairOut pair}</small> / #
        #{cents2dblT e}&nbsp;<small>#{renderPairOut pair}</small>
        <br>
        <small .text-muted>
            #{renderDateTimeRow l tzo t []}
        |]
    centsLeft = exchangeOrderAmountLeft order
    pair = exchangeOrderPair order

orderOperationTr :: ExchangePair -> Entity ExchangeOrderExecution -> Widget
orderOperationTr pair (Entity _ op) = do
    l <- liftHandler selectLocale
    tzo <- liftHandler timezoneOffsetFromCookie
    toWidget [whamlet|
        <tr .data-row>
            <td .text-center>
                <small>#{renderDateTimeRow l tzo (exchangeOrderExecutionTime op) []}
            <td>
                <small>_{MsgExchange} #
                <span>
                    <b>#{cents2dblT transfered}#
                    <small .text-muted>#{renderPairOut pair}
                $if exchangeOrderExecutionFullyExecuted op
                    <small> _{MsgOrderWasExecuted} #
            <td .text-center .align-middle>
                +#
                <span>
                    <b>#{cents2dblT (income - fee)}#
                    <small .text-muted>#{renderPairIn pair}
                <br .d-md-none>
                <small .d-md-none>
                    -#
                    <span>#{cents2dblT fee}
                        <small .text-muted>#{renderPairIn pair}
            <td .d-none .d-md-table-cell .text-center .align-middle>
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
        <small>#{renderDateTimeRow l tzo time []}
        |]

renderMobileList :: [ Entity ExchangeOrder ] -> TimeLocale -> Int -> Widget
renderMobileList os _loc _off = do
    locale <- selectLocale
    tzo <- handlerToWidget timezoneOffsetFromCookie
    let groups = labeledGroups $ dateGroups tzo
    [whamlet|
        <div .container-fluid .d-md-none .order-list .mobile .active>
            $forall (d, g) <- groups
                <div .row .group-date>
                    <div
                        .col
                        .date
                        .text-center
                        .text-lowercase
                        .text-muted
                        .mb-2
                        .mt-4
                        >
                        <small>
                            #{renderDateRow locale tzo (dateFromDay d)}
                <div .row .order-list .group-view .client .mobile>
                    ^{children g}
        |]
    where
        children g = concat <$> mapM
            (\x -> [whamlet|<div .col-12 .mx-auto>
                    ^{renderMobileOrderCard x }|])
            g

        dateGroups tzo = groupBy (equalOrderEntityDate tzo) os

        labeledGroups = map unsafeLabelByHead

        unsafeLabelByHead xs@(Entity _ ExchangeOrder{..} : _) =
            (utctDay exchangeOrderCreated, xs)
        unsafeLabelByHead [] = error "Impossible happened: date grouped order lists must contain at least one element"

        dateFromDay d = UTCTime d (fromIntegral (0 :: Int))

renderMobileOrderCard :: Entity ExchangeOrder -> Widget
renderMobileOrderCard (Entity oid o@ExchangeOrder{..}) = [whamlet|
    <div
        .order-card
        .mobile
        :exchangeOrderIsActive:.active
        :isOrderExecuted o:.executed
        :isOrderCancelled o:.cancelled
        data-order=#{fromSqlKey oid}
        .container-fluid
        .my-1
        >
            <div .row>
                <div .col-3 .text-right>
                    <small>
                        #{dbl2MoneyT exchangeOrderNormalizedRatio}
                <div .col-5 .text-right>
                    <a href=@{ClientOrderViewR oid}>
                        <small>
                            #{cents2dblT exchangeOrderAmountCents}
                            <small .text-muted>#{currencySymbol outCurrency}
                <div .col-4 .text-right>
                    <a href=@{ClientOrderViewR oid}>
                        <small>
                            #{cents2dblT inAmountCents}&nbsp;#
                            <small .text-muted>#{currencySymbol inCurrency}
    |]
    where
        (outCurrency, inCurrency) = unPairCurrency exchangeOrderPair
        ratio = normalizeRatio
            exchangeOrderRatioNormalization
            exchangeOrderPair
            exchangeOrderNormalizedRatio
        inAmountCents = multiplyCents ratio exchangeOrderAmountCents

renderOrderRate :: ExchangeOrder -> Html
renderOrderRate order = [shamlet|
    #{format (fixed 2) rate}|]
  where
    rate = exchangeOrderNormalizedRatio order

renderPairOut :: ExchangePair -> Html
renderPairOut = toHtml . currencySymbol . fst . unPairCurrency

renderPairIn :: ExchangePair -> Html
renderPairIn = toHtml . currencySymbol . snd . unPairCurrency

renderOrderNRatioSign :: ExchangeOrder -> Html
renderOrderNRatioSign order = [shamlet|
    #{renderPairOut rn}/#{renderPairIn rn}
    |]
  where
    rn = exchangeOrderRatioNormalization order

-- | === Predicates

isOrderExecuted :: ExchangeOrder -> Bool
isOrderExecuted o = case exchangeOrderStatus o of
    Executed _ -> True
    _          -> False

isOrderCancelled :: ExchangeOrder -> Bool
isOrderCancelled o = case exchangeOrderStatus o of
    Cancelled _ -> True
    _           -> False

equalOrderEntityDate
    :: Int ->  Entity ExchangeOrder -> Entity ExchangeOrder -> Bool
equalOrderEntityDate tzo (Entity _ o1) (Entity _ o2) =
    let tzoPico = realToFrac tzo
        t = utctDay . addUTCTime tzoPico . exchangeOrderCreated
    in t o1 == t o2

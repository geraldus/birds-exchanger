{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Profile where

import           Import
import           Local.Persist.Currency ( Currency, currSign, currencyCodeT )
import           Local.Persist.Wallet
import           Utils.Common           ( selectLocale )
import           Utils.Money
import           Utils.Time

import           Data.Maybe             ( fromJust )
import           Database.Persist.Sql   ( Single (..), fromSqlKey, rawSql )
import           Text.Julius            ( RawJS (..) )


getProfileR :: Handler Html
getProfileR = do
    userName                     <- userNameF . snd <$> requireAuthPair
    (Entity clientId _, wallets) <- requireClientData
    let walletIds  = map entityKey wallets
        walletVals = map entityVal wallets
        findWallet :: UserWalletId -> Entity UserWallet
        findWallet wid =
            fromJust $ find (\(Entity uwid _) -> uwid == wid) wallets
        findWalletCurrency = userWalletCurrency . entityVal . findWallet
    -- Let JS Front-end take and visualize data
    walletOps <- runDB $ selectList
        [WalletBalanceTransactionWalletId <-. walletIds]
        [Desc WalletBalanceTransactionTime, Desc WalletBalanceTransactionId]
    operations <- runDB $ rawSql s [toPersistValue clientId]
    let ops
            :: [ ( Entity WalletBalanceTransaction
                 , Single Currency
                 , Entity WalletTransactionReason
                 )
               ]
        ops = operations
    let reasonIds :: [WalletTransactionReasonId]
        reasonIds = map (entityKey . thd3) operations
        reasonIds' = intercalate "," $ map (show . fromSqlKey) reasonIds
    (depositOps, osWithdrawal, osWithdrawalReject, exchangeOrderOps, exchangeExecutionOps, exchangeOrderCancellations) <-
        case reasonIds of
            [] -> pure ([], [], [], [], [], [])
            _ -> do
                dos <- runDB $ rawSql (ds . pack $ reasonIds') []
                    :: Handler [ (Entity DepositRequest, Entity AcceptedDeposit) ]
                wos <- runDB $ rawSql (ws . pack $ reasonIds') []
                    :: Handler [ Entity WithdrawalRequest ]
                wros <- runDB $ rawSql (wrs . pack $ reasonIds') []
                    :: Handler [ (Entity WithdrawalRequest, Entity WithdrawalReject) ]
                eoos <- runDB $ rawSql (eos . pack $ reasonIds') []
                    :: Handler [ Entity ExchangeOrder ]
                eoes <- runDB $ rawSql (ees . pack $ reasonIds') []
                    :: Handler [ Entity ExchangeOrderExecution ]
                eocs <- runDB $ rawSql (ecs . pack $ reasonIds') []
                    :: Handler [ Entity ExchangeOrderCancellation ]
                return (dos, wos, wros, eoos, eoes, eocs)
    dataTableId <- newIdent
    defaultLayout $ do
        setTitle . toHtml $ "Мой портфель | " <> userName
        $(widgetFile "profile")
  where
    s
        = "SELECT ??, user_wallet.currency, ?? FROM wallet_balance_transaction, user_wallet,  \
        \    wallet_transaction_reason \
        \ WHERE wallet_balance_transaction.wallet_id = user_wallet.id \
        \ AND wallet_balance_transaction.wallet_transaction_reason_id =  \
        \    wallet_transaction_reason.id \
        \ AND user_wallet.user_id = ? \
        \ ORDER BY wallet_balance_transaction.time DESC"
    ds _in
        = "SELECT ??, ?? FROM deposit_request, accepted_deposit \
        \ WHERE deposit_request.id = accepted_deposit.deposit_request_id \
        \ AND accepted_deposit.wallet_transaction_reason_id IN (" <> _in <>")"
    ws _in
        = "SELECT ?? FROM withdrawal_request \
        \ WHERE withdrawal_request.wallet_transaction_reason_id IN (" <> _in <>")"
    was _in
        = "SELECT ??, ?? FROM withdrawal_request, withdrawal_accept \
        \ WHERE withdrawal_request.id = withdrawal_accept.request_id \
        \ AND withdrawal_request.wallet_transaction_reason_id IN (" <> _in <>")"
    wrs _in
        = "SELECT ??, ?? FROM withdrawal_request, withdrawal_reject \
        \ WHERE withdrawal_request.id = withdrawal_reject.request_id \
        \ AND withdrawal_request.wallet_transaction_reason_id IN (" <> _in <>")"
    eos _in
        = "SELECT ?? FROM exchange_order \
        \ WHERE exchange_order.wallet_transaction_reason_id IN (" <> _in <>")"
    ees _in
        = "SELECT ?? FROM exchange_order_execution \
        \ WHERE exchange_order_execution.in_wallet_transaction_reason_id IN (" <> _in <>")"
    ecs _in
        = "SELECT ?? FROM exchange_order_cancellation \
        \ WHERE exchange_order_cancellation.reason_id IN (" <> _in <>")"
    isPosWithdrawal wbt = case walletBalanceTransactionType wbt of
        BalanceWithdrawal c -> c > 0
        _                   -> False
    thd3 :: (a, b, c) -> c
    thd3 (_, _, x) = x



transactionTr
    :: Entity WalletBalanceTransaction
    -> Currency
    -> [ (Entity DepositRequest, Entity AcceptedDeposit) ]
    -> [ Entity WithdrawalRequest ]
    -> [ ( Entity WithdrawalRequest, Entity WithdrawalReject ) ]
    -> [ Entity ExchangeOrder ]
    -> [ Entity ExchangeOrderExecution ]
    -> [ Entity ExchangeOrderCancellation ]
    -> Widget
transactionTr (Entity wbtId wbt) wbtCurrency drsAdrs wos wros eos ees ecs = do
    l <- liftHandler selectLocale
    tzo <- handlerToWidget timezoneOffsetFromCookie
    let timeT = renderDateTimeRow l tzo
    toWidget [whamlet|
        <tr #data-row-#{fromSqlKey wbtId} .data-row .deposit .#{wbtCC} .#{trType}>
            <td .text-muted>
                <small>
                    <small>
                        #{timeT wbtTime}
            ^{wbtDesc}
        |]
  where
    wbtType = walletBalanceTransactionType wbt
    wbtTime = walletBalanceTransactionTime wbt
    wbtCC = toHtml . toLower . currencyCodeT $ wbtCurrency
    wbtDesc = case wbtType of
        BalanceDeposit cents -> depositDesc wbt cents wbtCurrency drsAdrs
        BalanceWithdrawal cents -> withdrawalDesc wbt cents wbtCurrency wos
        BalanceWithdrawalCancel cents -> withdrawalRejectDesc wbt cents wbtCurrency wros
        ExchangeFreeze cents -> orderCreationDesc wbt cents wbtCurrency eos
        ExchangeExchange cents -> orderExecutionDesc wbt cents wbtCurrency ees
        ExchangeReturn cents -> orderCancelDesc wbt cents wbtCurrency ecs
        _ -> toWidget (mempty :: Html)
    trType = case wbtType of
        BalanceDeposit _          -> "deposit" :: Html
        BalanceWithdrawal _       -> "withdrawal" :: Html
        BalanceWithdrawalCancel _ -> "withdrawal" :: Html
        ExchangeFreeze _          -> "exchange-freeze" :: Html
        ExchangeExchange _        -> "exchange-exchange" :: Html
        _                         -> mempty :: Html

depositDesc
    :: WalletBalanceTransaction
    -> Int
    -> Currency
    -> [ (Entity DepositRequest, Entity AcceptedDeposit) ]
    -> Widget
depositDesc wbt cents c rsAws = toWidget
    [whamlet|
        <td>
            #{renderAmount cents c}
        <td>
            <span>_{MsgBalanceDeposit}#
            $maybe eRequest <- mDepositRequestE
                <span>: #
                <a href="@{DepositR}/#data-row-#{requestIdStr eRequest}" title="_{MsgViewRequestDetails}">
                    \_{MsgRequest} ##{requestIdStr eRequest}
        |]
  where
    reason = walletBalanceTransactionWalletTransactionReasonId wbt
    mbRsAs = find (\(_, Entity _ ad) -> acceptedDepositWalletTransactionReasonId ad == reason) rsAws
    mDepositRequestE = fmap fst mbRsAs
    requestIdStr :: Entity DepositRequest -> Text
    requestIdStr = pack . show . fromSqlKey . entityKey


withdrawalDesc
    :: WalletBalanceTransaction
    -> Int
    -> Currency
    -> [ Entity WithdrawalRequest ]
    -> Widget
withdrawalDesc wbt cents c rsAs =
    toWidget
        [whamlet|
            <td>
                #{renderAmount cents c}#
            <td>
                <span>_{MsgBalanceWithdrawal}#
                $maybe eRequest <- mbr
                    <span>: #
                    <a href="@{WithdrawalR}/#data-row-#{requestIdStr eRequest}" title="_{MsgViewRequestDetails}">
                        \_{MsgRequest} ##{requestIdStr eRequest}
            |]
  where
    reason = walletBalanceTransactionWalletTransactionReasonId wbt
    mbr = find (\(Entity _ wr) -> withdrawalRequestWalletTransactionReasonId wr == reason) rsAs
    requestIdStr :: Entity WithdrawalRequest -> Text
    requestIdStr = pack . show . fromSqlKey . entityKey

withdrawalRejectDesc
    :: WalletBalanceTransaction
    -> Int
    -> Currency
    -> [ ( Entity WithdrawalRequest, Entity WithdrawalReject ) ]
    -> Widget
withdrawalRejectDesc wbt cents c rs =
    toWidget
        [whamlet|
            <td>
                #{renderAmount cents c}#
            <td>
                <span>_{MsgBalanceReturn}#
                $maybe eRequest <- fmap fst mbr
                    <span>: #
                    <a href="@{WithdrawalR}/#data-row-#{requestIdStr eRequest}" title="_{MsgViewRequestDetails}">
                        \_{MsgRequest} ##{requestIdStr eRequest}
            |]
  where
    reason = walletBalanceTransactionWalletTransactionReasonId wbt
    mbr = find (\(_, Entity _ wr) -> withdrawalRejectTransactionReasonId wr == reason) rs
    requestIdStr :: Entity WithdrawalRequest -> Text
    requestIdStr = pack . show . fromSqlKey . entityKey

orderCreationDesc
    :: WalletBalanceTransaction
    -> Int
    -> Currency
    -> [ Entity ExchangeOrder ]
    -> Widget
orderCreationDesc wbt cents c eos = toWidget
    [whamlet|
        <td>
            #{renderAmount cents c}#
        <td>
            <span>_{MsgOrderCreated}#

            $maybe eRequest@(Entity eid _) <- mRequestE
                <span>: #
                <a href="@{ClientOrderViewR eid}" title="_{MsgViewOrderDetails}">
                    \_{MsgOrder} ##{requestIdStr eRequest}
        |]
  where
    reason = walletBalanceTransactionWalletTransactionReasonId wbt
    mRequestE = find (\(Entity _ eo) -> exchangeOrderWalletTransactionReasonId eo == reason) eos
    requestIdStr :: Entity ExchangeOrder -> Text
    requestIdStr = pack . show . fromSqlKey . entityKey

orderCancelDesc
    :: WalletBalanceTransaction
    -> Int
    -> Currency
    -> [ Entity ExchangeOrderCancellation ]
    -> Widget
orderCancelDesc wbt cents c ecs =
    toWidget
        [whamlet|
            <td>
                #{renderAmount cents c}#
            <td>
                <span>_{MsgOrderCancellation}#

                $maybe eRequest@(Entity _ ec) <- mRequestE
                    <span>: #
                    <a href="@{ClientOrderViewR (exchangeOrderCancellationOrderId ec)}" title="_{MsgViewOrderDetails}">
                        \_{MsgOrder} ##{requestIdStr eRequest}
            |]
  where
    reason = walletBalanceTransactionWalletTransactionReasonId wbt
    mRequestE = find (\(Entity _ eo) -> exchangeOrderCancellationReasonId eo == reason) ecs
    requestIdStr :: Entity ExchangeOrderCancellation -> Text
    requestIdStr = pack . show . fromSqlKey . exchangeOrderCancellationOrderId . entityVal

orderExecutionDesc
    :: WalletBalanceTransaction
    -> Int
    -> Currency
    -> [ Entity ExchangeOrderExecution ]
    -> Widget
orderExecutionDesc wbt cents c ees = toWidget
    [whamlet|
        <td>
            #{renderAmount cents c}#
        <td>
            <span>_{MsgOrderExecution}#

            $maybe eRequest@(Entity _ e) <- mRequestE
                <span>: #
                <a href="@{ClientOrderViewR (exchangeOrderExecutionOrderId e)}" title="_{MsgViewOrderDetails}">
                    \_{MsgOrder} ##{requestIdStr eRequest}
        |]
  where
    reason = walletBalanceTransactionWalletTransactionReasonId wbt
    mRequestE = find (\(Entity _ ee) -> exchangeOrderExecutionInWalletTransactionReasonId ee == reason) ees
    requestIdStr :: Entity ExchangeOrderExecution -> Text
    requestIdStr = pack . show . fromSqlKey . entityKey



renderAmount :: Int -> Currency -> Html
renderAmount amt cur = [shamlet|
    <span>
        <big>
            #{centsT}#
            <small .text-muted>
                \#{sign} |]
  where
    centsT' = cents2dblT amt
    prefix = if amt > 0 then "+" else ""
    centsT = prefix <> centsT'
    sign = currSign cur


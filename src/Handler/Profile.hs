{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Profile where

import           Import
import           Local.Persist.Currency ( Currency, currSign, currencyCodeT )
import           Local.Persist.Wallet
import           Utils.Time             ( renderDateTimeRow )

import           Data.Maybe             ( fromJust )
import           Data.Tuple.Extra       ( fst3, thd3 )
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
-- (Entity _ (op :: WalletBalanceTransaction), Single (c :: Currency), Entity _ (reason :: WalletTransactionReason))
        ops = operations
    let reasonIds :: [WalletTransactionReasonId]
        reasonIds = map (entityKey . thd3) operations
        reasonIds' = intercalate "," $ map (show . fromSqlKey) reasonIds
    depositOps <- runDB $ rawSql (ds . pack $ reasonIds') []
            :: Handler [ (Entity DepositRequest, Entity AcceptedDeposit) ]

    withdrawalOps <- runDB $ rawSql (ws . pack $ reasonIds') []
            :: Handler [ (Entity WithdrawalRequest, Entity WithdrawalAccept) ]
    let targets = filter (\(Entity _ wbt) -> isPosWithdrawal wbt) (map fst3 ops)
    runDB $ mapM_ (\(Entity wbtid wbt) -> do
                let amt = case walletBalanceTransactionType wbt of
                            BalanceWithdrawal c -> negate c
                            _                   -> error "Shouldn't be"
                update wbtid [ WalletBalanceTransactionType =. BalanceWithdrawal amt ])
        targets

    exchangeOrderOps <- runDB $ rawSql (eos . pack $ reasonIds') []
            :: Handler [ Entity ExchangeOrder ]
    exchangeExectutionOps <- runDB $ rawSql (ees . pack $ reasonIds') []
            :: Handler [ Entity ExchangeOrderExecution ]

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
        = "SELECT ??, ?? FROM withdrawal_request, withdrawal_accept \
        \ WHERE withdrawal_request.id = withdrawal_accept.request_id \
        \ AND withdrawal_request.wallet_transaction_reason_id IN (" <> _in <>")"
    eos _in
        = "SELECT ?? FROM exchange_order \
        \ WHERE exchange_order.wallet_transaction_reason_id IN (" <> _in <>")"
    ees _in
        = "SELECT ?? FROM exchange_order_execution \
        \ WHERE exchange_order_execution.in_wallet_transaction_reason_id IN (" <> _in <>")"
    isPosWithdrawal wbt = case walletBalanceTransactionType wbt of
        BalanceWithdrawal c -> c > 0
        _                   -> False



transactionTr
    :: Entity WalletBalanceTransaction
    -> Currency
    -> [ (Entity DepositRequest, Entity AcceptedDeposit) ]
    -> [ (Entity WithdrawalRequest, Entity WithdrawalAccept) ]
    -> [ Entity ExchangeOrder ]
    -> [ Entity ExchangeOrderExecution ]
    -> Widget
transactionTr (Entity wbtId wbt) wbtCurrency drsAdrs wrsAwrs eos ees = do
    l <- liftHandler selectLocale
    let timeT = renderDateTimeRow l
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
        BalanceWithdrawal cents -> withdrawalDesc wbt cents wbtCurrency wrsAwrs
        ExchangeFreeze cents -> orderCreationDesc wbt cents wbtCurrency eos
        ExchangeExchange cents -> orderExecutionDesc wbt cents wbtCurrency ees
        _ -> toWidget (mempty :: Html)
    trType = case wbtType of
        BalanceDeposit _    -> "deposit" :: Html
        BalanceWithdrawal _ -> "withdrawal" :: Html
        ExchangeFreeze _    -> "exchange-freeze" :: Html
        ExchangeExchange _  -> "exchange-exchange" :: Html
        _                   -> mempty :: Html

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
                <a href="#" title="_{MsgViewRequestDetails}">
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
    -> [ (Entity WithdrawalRequest, Entity WithdrawalAccept) ]
    -> Widget
withdrawalDesc wbt cents c rsAs =
    toWidget
        [whamlet|
            <td>
                #{renderAmount cents c}#
            <td>
                <span>_{MsgBalanceWithdrawal}#
                $maybe eRequest <- mRequestE
                    <span>: #
                    <a href="#" title="_{MsgViewRequestDetails}">
                        \_{MsgRequest} ##{requestIdStr eRequest}
            |]
  where
    reason = walletBalanceTransactionWalletTransactionReasonId wbt
    mbRsAs = find (\(Entity _ wr, _) -> withdrawalRequestWalletTransactionReasonId wr == reason) rsAs
    mRequestE = fmap fst mbRsAs
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


{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Profile where

import           Import
import           Local.Persist.Currency     ( Currency, currSign, currencyCodeT,
                                              currencyCodeT', currencyNameT,
                                              currencySymbol )
import           Local.Persist.Wallet
import           Type.Wallet                ( WalletData (..) )
import           Utils.App.Client
import           Utils.Common               ( selectLocale )
import           Utils.Database.User.Wallet
import           Utils.Money
import           Utils.Render               ( renderCurrencyAmount )
import           Utils.Time                 ( renderDateRow,
                                              timezoneOffsetFromCookie )

import           Data.Time.Clock            ( addUTCTime )
import           Data.Time.Clock.POSIX      ( utcTimeToPOSIXSeconds )
import           Database.Persist.Sql       ( Single (..), fromSqlKey, rawSql )
import           Text.Julius                ( RawJS (..) )


getProfileR :: Handler Html
getProfileR = do
    _userName <- userNameF . snd <$> requireAuthPair
    locale <- selectLocale
    tzo <- timezoneOffsetFromCookie
    (Entity clientId _, wallets) <- requireClientData
    let walletVals = map entityVal wallets
    operations <- runDB $ rawSql s [toPersistValue clientId]
    let ops
            :: [ ( Entity WalletBalanceTransaction
                 , Single Currency
                 , Entity WalletTransactionReason
                 )
               ]
        ops = operations
    let dateGroupedOps = groupBy (transactionDay tzo) ops
    let labeledDateGroupedOps = map (labelGroupUnsafe tzo) dateGroupedOps
    let reasonIds :: [WalletTransactionReasonId]
        reasonIds = map (entityKey . thd3) operations
        reasonIds' = intercalate "," $ map (show . fromSqlKey) reasonIds
    (depositOps, osWithdrawal, osWithdrawalCancel, osWithdrawalReject, exchangeOrderOps, exchangeExecutionOps, exchangeOrderCancellations) <-
        case reasonIds of
            [] -> pure ([], [], [], [], [], [], [])
            _ -> do
                dos <- runDB $ rawSql (ds . pack $ reasonIds') []
                    :: Handler [ (Entity DepositRequest, Entity AcceptedDeposit) ]
                wos <- runDB $ rawSql (ws . pack $ reasonIds') []
                    :: Handler [ Entity WithdrawalRequest ]
                wcos <- runDB $ rawSql (wcs . pack $ reasonIds') []
                    :: Handler [ (Entity WithdrawalRequest, Entity WithdrawalCancel) ]
                wros <- runDB $ rawSql (wrs . pack $ reasonIds') []
                    :: Handler [ (Entity WithdrawalRequest, Entity WithdrawalReject) ]
                eoos <- runDB $ rawSql (eos . pack $ reasonIds') []
                    :: Handler [ Entity ExchangeOrder ]
                eoes <- runDB $ rawSql (ees . pack $ reasonIds') []
                    :: Handler [ Entity ExchangeOrderExecution ]
                eocs <- runDB $ rawSql (ecs . pack $ reasonIds') []
                    :: Handler [ Entity ExchangeOrderCancellation ]
                return (dos, wos, wcos, wros, eoos, eoes, eocs)
    dataTableId <- newIdent
    defaultLayout $ do
        setAppPageTitle MsgClientProfilePageTitle
        let walletTotals = accountWalletStats wallets
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
    wcs _in
        = "SELECT ??, ?? FROM withdrawal_request, withdrawal_cancel \
        \ WHERE withdrawal_request.id = withdrawal_cancel.request_id \
        \ AND withdrawal_cancel.transaction_reason_id IN (" <> _in <>")"
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

    thd3 :: (a, b, c) -> c
    thd3 (_, _, x) = x

    labelGroupUnsafe _ [] = (error "Shouldn't happen.  No items in group", [])
    labelGroupUnsafe timezoneSeconds gs@((Entity _ t, _, _) : _) = let
        trTime = addUTCTime
                (realToFrac timezoneSeconds) (walletBalanceTransactionTime t)
        d = utctDay trTime
        in (UTCTime d (fromRational 0), gs)

    utcDayTZ pico = utctDay . addUTCTime pico . walletBalanceTransactionTime

    transactionDay timezoneSeconds (Entity _ t1, _, _) (Entity _ t2, _, _) = let
            tzoPico = realToFrac timezoneSeconds
            d1 = utcDayTZ tzoPico t1
            d2 = utcDayTZ tzoPico t2
        in d1 == d2


transactionTr ::
       Entity WalletBalanceTransaction
    -> Currency
    -> [ (Entity DepositRequest, Entity AcceptedDeposit) ]
    -> [ Entity WithdrawalRequest ]
    -> [ ( Entity WithdrawalRequest, Entity WithdrawalCancel ) ]
    -> [ ( Entity WithdrawalRequest, Entity WithdrawalReject ) ]
    -> [ Entity ExchangeOrder ]
    -> [ Entity ExchangeOrderExecution ]
    -> [ Entity ExchangeOrderCancellation ]
    -> Widget
transactionTr (Entity wbtId wbt) wbtCurrency drsAdrs wos wcos wros eos ees ecs =
    toWidget [whamlet|
        <tr #data-row-#{fromSqlKey wbtId} .data-row .deposit .#{wbtCC} .#{trType}>
            <td .d-none .d-md-table-cell>
                ^{dateTimeRowW wbtTime}
            ^{wbtDesc}
        |]
  where
    wbtType = walletBalanceTransactionType wbt
    wbtTime = walletBalanceTransactionTime wbt
    wbtCC = toHtml . toLower . currencyCodeT $ wbtCurrency
    wbtDesc = case wbtType of
        BalanceDeposit cents -> depositDesc wbt cents wbtCurrency drsAdrs
        BalanceWithdrawal cents -> withdrawalDesc wbt cents wbtCurrency wos
        BalanceWithdrawalCancel cents ->
            withdrawalCancelDesc wbt cents wbtCurrency wcos
        BalanceWithdrawalReject cents ->
            withdrawalRejectDesc wbt cents wbtCurrency wros
        ExchangeFreeze cents -> orderCreationDesc wbt cents wbtCurrency eos
        ExchangeExchange cents -> orderExecutionDesc wbt cents wbtCurrency ees
        ExchangeReturn cents -> orderCancelDesc wbt cents wbtCurrency ecs
        ParaMining cents -> paraMiningDesc wbt cents wbtCurrency
    trType :: Html
    trType = case wbtType of
        BalanceDeposit _          -> "deposit"
        BalanceWithdrawal _       -> "withdrawal"
        BalanceWithdrawalCancel _ -> "withdrawal"
        BalanceWithdrawalReject _ -> "withdrawal"
        ExchangeFreeze _          -> "exchange-freeze"
        ExchangeExchange _        -> "exchange-exchange"
        _                         -> mempty

depositDesc ::
       WalletBalanceTransaction
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


withdrawalDesc ::
       WalletBalanceTransaction
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

withdrawalCancelDesc ::
        WalletBalanceTransaction
        -> Int
        -> Currency
        -> [ ( Entity WithdrawalRequest, Entity WithdrawalCancel ) ]
        -> Widget
withdrawalCancelDesc wbt cents c rs =
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
    mbr = find (\(_, Entity _ wc) -> withdrawalCancelTransactionReasonId wc == reason) rs
    requestIdStr :: Entity WithdrawalRequest -> Text
    requestIdStr = pack . show . fromSqlKey . entityKey

orderCreationDesc ::
       WalletBalanceTransaction
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

orderCancelDesc ::
       WalletBalanceTransaction
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
                    <a
                        href="@{ClientOrderViewR (exchangeOrderCancellationOrderId ec)}"
                        title="_{MsgViewOrderDetails}"
                        >
                        \_{MsgOrder} ##{requestIdStr eRequest}
            |]
  where
    reason = walletBalanceTransactionWalletTransactionReasonId wbt
    mRequestE = find (\(Entity _ eo) -> exchangeOrderCancellationReasonId eo == reason) ecs
    requestIdStr :: Entity ExchangeOrderCancellation -> Text
    requestIdStr = pack . show . fromSqlKey . exchangeOrderCancellationOrderId . entityVal

orderExecutionDesc ::
       WalletBalanceTransaction
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
    requestIdStr =
            pack . show . fromSqlKey . exchangeOrderExecutionOrderId . entityVal

paraMiningDesc ::
       WalletBalanceTransaction
    -> Int
    -> Currency
    -> Widget
paraMiningDesc _ cents c = toWidget
    [whamlet|
        <td>#{renderAmount cents c}
        <td><span>_{MsgParaMining}
        |]

accountWalletStats :: [Entity UserWallet] -> Widget
accountWalletStats ws = $(widgetFile "client/wallet/account-stats")

walletStats :: Entity UserWallet -> Widget
walletStats wal@(Entity _ w) = do
    now <- liftIO getCurrentTime
    (os, rs, pt) <- handlerToWidget . runDB $ do
        os'   <- getUserWalletActiveOrders wal
        rs'   <- getUserWalletActiveWithdrawal wal
        paraTime' <- lastWalletStrictParaTimeDB wal
        return (os', rs', paraTime')
    currencyRender <- handlerToWidget getAmountRenderer
    let wc = userWalletCurrency w
        stats = foldUserWalletStats wal os rs pt
        walCents = userWalletAmountCents w
        ordCents = walletDataOrdersCents stats
        wreqCents = walletDataWithdrawalCents stats
        cents = walCents + ordCents + wreqCents
        currencyCode = (toLower . currencyCodeT') wc
        currencyName = currencyNameT wc
        walletCents =
            currencyRender [] [] False wc walCents
        orderCents =
            currencyRender [] [] False wc ordCents
        withdrawalCents =
            currencyRender [] [] False wc wreqCents
        lastParaTime = pt
        paraCents = lastParaTime >>= \t ->
            if cents < 1
            then
                Just (0, 0, t)
            else
                (\(v, k) -> (v, k, t)) <$> currencyAmountPara now t wc cents

        paraStats :: Widget
        paraStats = maybe
                mempty
                (\(v, k, time) -> [whamlet|
                    <div title="_{MsgParaMining}" .wallet .paramining .#{currencyCode}>
                        <i .fas .fa-angle-double-up .stats-icon>
                        <span .value .para .#{currencyCodeT' wc}>
                            #{fixedDoubleT 7 (v / 100)}
                        <small .currency .para .#{currencyCodeT' wc} .text-muted>
                            #{currencySymbol wc}
                    <script>
                        window['para_#{currencyCodeT' wc}'] = {
                            timestamp: #{fixedDoubleT 12 (realToFrac (utcTimeToPOSIXSeconds time) * 1000)},
                            amount: #{show cents},
                            k_s: #{fixedDoubleT 12 (realToFrac k)}
                        };
                    |])
                paraCents
    $(widgetFile "client/wallet/stats")


-- TODO: Use getAmountRender from Foundation
renderAmount :: Int -> Currency -> Html
renderAmount amt cur =
    renderCurrencyAmount (error "no locale") [] ["value"] True cur amt

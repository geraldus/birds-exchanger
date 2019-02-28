{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Handler.Client.Withdrawal where

import           Import

import           Form.Profile.Withdrawal
import           Local.Persist.Currency  ( currSign )
import           Local.Persist.Wallet
import           Type.Money              ( Money (..) )
import           Type.Withdrawal
import           Utils.I18n
import           Utils.Time              ( renderDateTimeRow )

import           Database.Persist.Sql    ( fromSqlKey )


getWithdrawalR :: Handler Html
getWithdrawalR = do
    requireClientId
    formId <- newIdent
    (widget, enctype) <- generateFormPost $ withdrawalForm formId
    defaultLayout $ defaultWidget formId widget enctype Nothing

postWithdrawalCreateR :: Handler Html
postWithdrawalCreateR = do
    requireClientId
    formId <- newIdent
    ((res, widget), enctype) <- runFormPost $ withdrawalForm formId
    userId <- requireClientId
    let mayError = case res of
            FormSuccess _ -> Nothing
            FormMissing   -> Just  ["Не получены данные формы"]
            FormFailure e -> Just e
    case res of
        FormMissing -> defaultLayout $ defaultWidget formId widget enctype mayError
        FormFailure _ -> defaultLayout $ defaultWidget formId widget enctype mayError
        FormSuccess (WithdrawalM (Money amt c) tm fee adr) -> do
            -- TODO: FIXME:  Check if Transfer Method is valid
            w <- getOrCreateWallet userId c
            let wid = entityKey w
            let walletCents = userWalletAmountCents (entityVal w)
            let amount2Freeze = amt + fee
            if amount2Freeze > walletCents
                then defaultLayout $
                        defaultWidget
                            formId
                            widget
                            enctype
                            (Just ["Недостаточно средств на счёте"])
                else do
                    time <- liftIO getCurrentTime
                    reasonId <- runDB . insert $ WalletTransactionReason wid
                    let record = WithdrawalRequest
                            wid
                            tm
                            adr
                            amt
                            amount2Freeze
                            fee
                            time
                            reasonId
                            Nothing
                    let transaction = WalletBalanceTransaction
                            wid
                            (BalanceWithdrawal (negate amount2Freeze))
                            reasonId
                            walletCents
                            time
                    _ <- runDB $ do
                        insert record
                        insert transaction
                        update wid [UserWalletAmountCents -=. amount2Freeze]
                    setMessage "Заявка на вывод успешно создана"
                    redirect WithdrawalR


defaultWidget :: Text -> Widget -> Enctype -> Maybe [Text] -> Widget
defaultWidget formId widget enctype mayError = [whamlet|
    $maybe error <- mayError
        <div .row>
            <div .col-10 .mx-auto>
                <div .alert.alert-warning>
                    $forall e <- error
                        <div .error>#{e}
    <form ##{formId} method=post enctype=#{enctype} action=@{WithdrawalCreateR} .col-12 .col-sm-10 .col-md-8 .mx-auto>
        ^{widget}
        <div .form-group .row .justify-content-center>
            <button .btn.btn-lg.btn-outline-primary .mt-2 type=submit>вывод
    ^{withdrawalHistory}
    |]

withdrawalHistory :: Widget
withdrawalHistory = do
    (_, wallets) <- handlerToWidget requireClientData
    let walletIds = map entityKey wallets
    withdrawalOps <- handlerToWidget . runDB $ selectList [ WithdrawalRequestWalletId <-. walletIds ] [ ]
    let withdrawalIds = map entityKey withdrawalOps
    acceptedOps <- handlerToWidget . runDB $ selectList [ WithdrawalAcceptRequestId <-. withdrawalIds ] [ ]
    toWidget [whamlet|
        <table .table .table-striped .mt-5>
            <thead .thead-light>
                <th .align-top>_{MsgDateCreated}
                <th .align-top>
                    _{MsgDepositAmount}
                    <br>
                    <small .text-muted>
                        _{MsgTransferMethod}
                <th .align-top>
                    _{MsgFee}
                <th .align-top>
                    _{MsgDetails}
            <tbody>
                $forall op <- withdrawalOps
                    ^{withdrawalHistoryRow op acceptedOps}
        |]


withdrawalHistoryRow :: Entity WithdrawalRequest -> [ Entity WithdrawalAccept ] -> Widget
withdrawalHistoryRow (Entity ident request@WithdrawalRequest{..}) aos = do
    l <- handlerToWidget selectLocale
    tzo <- liftHandler timezoneOffsetFromCookie
    wallet <- handlerToWidget . runDB $ get404 withdrawalRequestWalletId
    let ew = Entity withdrawalRequestWalletId wallet
    toWidget [whamlet|
        <tr .data-row #data-row-#{fromSqlKey ident}>
            <td>
                <small .text-muted>
                    #{renderDateTimeRow l tzo withdrawalRequestCreated}
            <td .align-middle>
                #{cents2dblT withdrawalRequestCentsAmount}#
                <small .text-muted>
                    #{currSign (userWalletCurrency wallet)}
                <br>
                <small .text-muted>
                    _{transferMethodMsg withdrawalRequestMethod}
            <td .align-middle>
                ^{renderExpected ew}
            <td .align-middle>
                ^{renderStatus}
        |]
    where
        renderExpected :: Entity UserWallet -> Widget
        renderExpected ew = case find byIdent aos of
            Nothing -> do
                let expectedCents = withdrawalRequestCentsAmount - withdrawalRequestFeeAmount
                    feeSign = if withdrawalRequestFeeAmount == 0 then "" else "-" :: String
                [whamlet|
                    <big>
                        <b>
                            #{cents2dblT expectedCents}#
                        <small .text-muted>
                            #{currSign (userWalletCurrency (entityVal ew))}
                    <br>
                    <small .text-muted>
                        #{feeSign}#{cents2dblT withdrawalRequestFeeAmount}#
                        <small>
                            #{currSign (userWalletCurrency (entityVal ew))}
                    |]
            Just (Entity _ a) -> do
                let expectedCents = withdrawalAcceptAmountTransfered a - withdrawalAcceptActualFee a
                    feeSign = if withdrawalAcceptActualFee a == 0 then "" else "-" :: String
                [whamlet|
                    <big>
                        <b>
                            #{cents2dblT expectedCents}#
                        <small .text-muted>
                            #{currSign (userWalletCurrency (entityVal ew))}
                    <br>
                    <small .text-muted>
                        #{feeSign}#{cents2dblT (withdrawalAcceptActualFee a)}#
                        <small>
                            #{currSign (userWalletCurrency (entityVal ew))}
                        \ (по факту)
                    |]
        renderStatus :: Widget
        renderStatus = case withdrawalRequestAccepted of
            Nothing -> [whamlet|_{MsgAwaitingExecution}|]
            Just executed -> do
                l <- handlerToWidget selectLocale
                tzo <- liftHandler timezoneOffsetFromCookie
                [whamlet|
                    <p .text-uppercase>
                        _{MsgDepositExecuted}
                        <br>
                        <small .text-muted>
                            $case withdrawalRequestAccepted
                                $of Just time
                                    #{renderDateTimeRow l tzo time}
                                $of Nothing
                                    |]
            x -> [whamlet|#{show x}|]
        byIdent (Entity _ a) = ident == withdrawalAcceptRequestId a

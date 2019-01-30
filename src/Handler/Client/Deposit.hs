{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Handler.Client.Deposit where

import           Import

import           Form.Profile.Deposit
import           Local.Persist.Deposit
import           Utils.Time             ( renderDateTimeRow )
import           Local.Persist.Currency ( currSign )
import Utils.I18n

import           Database.Persist.Sql   ( fromSqlKey )


getDepositR :: Handler Html
getDepositR = do
    requireClientId
    formId <- newIdent
    (widget, enctype) <- generateFormPost $ depositForm formId
    defaultLayout $ defaultWidget formId widget enctype Nothing

postDepositR :: Handler Html
postDepositR = do
    userId <- requireClientId
    formId <- newIdent
    ((res, widget), enctype) <- runFormPost $ depositForm formId
    let mayError = case res of
            FormSuccess _ -> Nothing
            FormMissing   -> Just  ["Не получены данные формы"]
            FormFailure e -> Just e
    case res of
        FormMissing -> defaultLayout $ defaultWidget formId widget enctype mayError
        FormFailure _ -> defaultLayout $ defaultWidget formId widget enctype mayError
        FormSuccess DepositRequestFD{..} -> do
            code <- appNonce128urlT
            time <- liftIO getCurrentTime
            let depReqRecord = DepositRequest
                    depReqCurrency
                    depReqTransferMethod
                    depReqCentsAmount
                    depReqCentsExpectedFee
                    code
                    depReqTargetCurrency
                    depReqExpectedConversionRatio
                    time
                    userId
                    New
                    False
            _ <- runDB $ insert depReqRecord
            redirect $ DepositRequestConfirmationR code

defaultWidget :: Text -> Widget -> Enctype -> Maybe [Text] -> Widget
defaultWidget formId widget enctype mayError = [whamlet|
    <form ##{formId} method=post enctype=#{enctype} .col-12 .col-sm-10 .col-md-8 .mx-auto>
        ^{widget}
        $maybe error <- mayError
            <div .alert .alert-danger role="alert">
                $forall e <- error
                    <div .error>#{e}
        <div .form-group .row>
            <button type=submit .btn.btn-outline-primary.btn-lg .mx-auto>продолжить
    ^{depositHistory}
    |]

depositHistory :: Widget
depositHistory = do
    clientId <- handlerToWidget requireClientId
    depositOps <- handlerToWidget . runDB $ selectList [ DepositRequestUserId ==. clientId ] [ ]
    let depositIds = map entityKey depositOps
    acceptedOps <- handlerToWidget . runDB $ selectList [ AcceptedDepositDepositRequestId <-. depositIds ] [ ]
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
                    _{MsgDepositRealAmount}
                    <br>
                    <small .text-muted>
                        _{MsgFee}
                <th .align-top>
                    _{MsgDetails}
            <tbody>
                $forall op <- depositOps
                    ^{depositHistoryRow op acceptedOps}
        |]


depositHistoryRow :: Entity DepositRequest -> [ Entity AcceptedDeposit ] -> Widget
depositHistoryRow (Entity ident request@DepositRequest{..}) aos = do
    l <- handlerToWidget selectLocale
    toWidget [whamlet|
        <tr .data-row #data-row-#{fromSqlKey ident}>
            <td>
                <small .text-muted>
                    #{renderDateTimeRow l depositRequestCreated}
            <td .align-middle>
                #{cents2dblT depositRequestCentsAmount}#
                <small .text-muted>
                    #{currSign depositRequestCurrency}
                <br>
                <small .text-muted>
                    _{transferMethodMsg depositRequestTransferMethod}
            <td .align-middle>
                ^{renderExpected}
            <td .align-middle>
                ^{renderStatus}
        |]
    where
        renderExpected :: Widget
        renderExpected = case find byIdent aos of
            Nothing -> do
                let expectedCents = depositRequestCentsAmount - depositRequestExpectedFeeCents
                    feeSign = if depositRequestExpectedFeeCents == 0 then "" else "-" :: String
                [whamlet|
                    <big>
                        <b>
                            #{cents2dblT expectedCents}#
                        <small .text-muted>
                            #{currSign depositRequestCurrency}
                    <br>
                    <small .text-muted>
                        #{feeSign}#{cents2dblT depositRequestExpectedFeeCents}#
                        <small>
                            #{currSign depositRequestCurrency}
                    |]
            Just (Entity _ a) -> do
                let expectedCents = acceptedDepositCentsRealIncome a - acceptedDepositCentsActualFee a
                    feeSign = if acceptedDepositCentsActualFee a == 0 then "" else "-" :: String
                [whamlet|
                    <big>
                        <b>
                            #{cents2dblT expectedCents}#
                        <small .text-muted>
                            #{currSign depositRequestCurrency}
                    <br>
                    <small .text-muted>
                        #{feeSign}#{cents2dblT (acceptedDepositCentsActualFee a)}#
                        <small>
                            #{currSign depositRequestCurrency}
                        \ (по факту)
                    |]
        renderStatus :: Widget
        renderStatus = case depositRequestStatus of
            New -> [whamlet|
                    <a href=@{DepositRequestConfirmationR depositRequestTransactionCode}>
                        _{MsgDepositConfirmTransfer}
                    <p>
                        <i .text-muted>
                            <small>_{MsgAwaitingConfirmation}|]
            ClientConfirmed -> [whamlet|
                    <p>
                        _{MsgDepositConfirmed}
                        <br>
                        <i .text-muted>
                            <small>_{MsgAwaitingExecution}|]
            OperatorAccepted _ -> do
                l <- handlerToWidget selectLocale
                [whamlet|
                    <p .text-uppercase>
                        _{MsgDepositExecuted}
                        $case find byIdent aos
                            $of Just (Entity _ a)
                                <br>
                                    <small .text-muted>
                                        #{renderDateTimeRow l (acceptedDepositAccepted a)}
                            $of Nothing
                                |]
            x -> [whamlet|#{show x}|]
        byIdent (Entity _ a) = ident == acceptedDepositDepositRequestId a
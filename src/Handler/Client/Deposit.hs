{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Handler.Client.Deposit where

import           Import                 hiding ( on, (==.) )

import           Form.Profile.Deposit
import           Local.Persist.Currency ( Currency (..), currSign )
import           Local.Persist.Wallet   ( DepositRequestStatus (..) )
import           Utils.App.Client
import           Utils.App.Common
import           Utils.Common
import           Utils.I18n
import           Utils.Money

import           Database.Esqueleto
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
defaultWidget formId widget enctype mayError = do
    setAppPageTitle MsgClientDepositPageTitle
    messageRender <- liftHandler getMessageRender
    $(widgetFile "client/request/common")
    [whamlet|
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


type DepositDetails =
    ( Entity DepositRequest
    , Maybe (Entity AcceptedDeposit)
    , Maybe (Entity DepositReject) )

data Details
    = NoDetails (Entity DepositRequest)
    | AcceptD (Entity DepositRequest) (Entity AcceptedDeposit)
    | RejectD (Entity DepositRequest) (Entity DepositReject)
    deriving Show

depositHistory :: Widget
depositHistory = do
    clientId <- handlerToWidget requireClientId
    depositDetails <- liftHandler . runDB . select $
        from $ \(r `LeftOuterJoin` macc `LeftOuterJoin` mrej) -> do
            on (just (r ^. DepositRequestId) ==. mrej ?. DepositRejectRequestId)
            on (just (r ^. DepositRequestId) ==. macc ?. AcceptedDepositDepositRequestId)
            where_ (r ^. DepositRequestUserId ==. val clientId)
            orderBy [desc (r ^. DepositRequestCreated)]
            return (r, macc, mrej)
    let list = map wrapDetails depositDetails
    toWidget [whamlet|
        <table .table .table-striped .mt-5>
            <thead .thead-light>
                <th .align-top>_{MsgDateCreated}
                <th .align-top>
                    _{MsgAmount}
                    <br>
                    <small .text-muted>
                        _{MsgTransferMethod}
                <th .align-top>
                    _{MsgDepositRealAmount}
                    <br>
                    <small .text-muted>
                        _{MsgFee}
                <th colspan=2 .align-top>
                    _{MsgDetails}
            <tbody>
                $forall r <- list
                    ^{depositHistoryRow r}
        |]
  where
    wrapDetails (r, Just accepted, _) = AcceptD r accepted
    wrapDetails (r, _, Just rejected) = RejectD r rejected
    wrapDetails (r, _, _)             = NoDetails r

unDetailsRequest :: Details -> Entity DepositRequest
unDetailsRequest (NoDetails r) = r
unDetailsRequest (AcceptD r _) = r
unDetailsRequest (RejectD r _) = r

depositHistoryRow :: Details -> Widget
depositHistoryRow d = do
    let r = unDetailsRequest d
    (ur, mr) <- getRenders
    fd <- getFormatDateRender
    ft <- getFormatTimeRender
    genericRow
        r
        (genericRequestAmount (requestAmounts d) (description d))
        (genericRequestStatus (requestStatuses ur mr (fd, ft) d))
  where
    description d = case d of
        NoDetails _ -> mempty
        _           -> [whamlet|\ (_{MsgInFact})|]

genericRow :: Entity DepositRequest -> Widget -> Widget -> Widget
genericRow (Entity ident r@DepositRequest{..}) expected status =
    toWidget [whamlet|
        <tr .data-row #data-row-#{fromSqlKey ident}>
            <td>^{dateTimeRowW depositRequestCreated}
            <td .align-middle>
                #{cents2dblT depositRequestCentsAmount}#
                <small .text-muted>
                    #{currSign depositRequestCurrency}
                <br>
                <small .text-muted>
                    _{transferMethodMsg depositRequestTransferMethod}
            <td .align-middle>
                ^{expected}
            <td .align-middle>
                ^{status}
            <td .controls .align-middle>
                $if isNew r
                    <i .request-cancel-button .control .fas .fa-times-circle title=_{MsgCancelRequest}>
                    <form .request-cancel-form .d-none method=post action=@{ClientCancelDepositR}>
                        <input type=hidden name="request-id" value="#{fromSqlKey ident}">
        |]

requestAmounts :: Details -> (Int, Bool, Int, Currency)
requestAmounts (NoDetails (Entity _ DepositRequest{..})) =
    let amt = depositRequestCentsAmount - depositRequestExpectedFeeCents
        isCancelled = case depositRequestStatus of
            ClientCancelled _ -> True; _ -> False
    in (amt, isCancelled, depositRequestExpectedFeeCents, depositRequestCurrency)
requestAmounts (AcceptD (Entity _ r) (Entity _ AcceptedDeposit{..})) =
    let amt = acceptedDepositCentsRealIncome - acceptedDepositCentsActualFee
    in (amt, False, acceptedDepositCentsActualFee, depositRequestCurrency r)
requestAmounts (RejectD (Entity _ r) _) =
    (depositRequestCentsAmount r, True, 0, depositRequestCurrency r)

requestStatuses
    :: (Route App -> Text)
    -> (AppMessage -> Text)
    -> (UTCTime -> Html, UTCTime -> Html)
    -> Details
    -> (Widget, Widget, Widget)
requestStatuses ur mr fs (NoDetails (Entity _ r@DepositRequest{..})) =
    let (sm, dm) = requestStatusMessages mr r
        code = depositRequestTransactionCode
        (extra, status) = case depositRequestStatus of
            New ->
                ( [whamlet|
                    <a href=#{ur (DepositRequestConfirmationR code)}>
                        #{mr MsgDepositConfirmTransfer}|]
                , mempty)
            _ -> (mempty, [whamlet|#{sm}<br>|])
        description = [whamlet|<i .text-muted><small>#{dm}|]
    in (extra, status, description)
requestStatuses
    _ mr (fd, ft) (AcceptD (Entity _ r) (Entity _ AcceptedDeposit{..})) =
        let (sm, _) = requestStatusMessages mr r
            status = [whamlet|#{sm}<br>|]
            description = [whamlet|<small .text-muted>
                #{fd acceptedDepositAccepted} #{ft acceptedDepositAccepted}|]
        in (mempty, status, description)
requestStatuses
    _ mr (fd, ft) (RejectD (Entity _ r) (Entity _ DepositReject{..})) =
        let (sm, _) = requestStatusMessages mr r
            status = [whamlet|#{sm}<br>|]
            description = [whamlet|<small .text-muted>
                #{fd depositRejectTime} #{ft depositRejectTime}
                <br>
                #{depositRejectReason}|]
        in (mempty, status, description)

requestStatusMessages :: (AppMessage -> Text) -> DepositRequest -> (Text, Text)
requestStatusMessages mr r = case depositRequestStatus r of
    New                -> (mempty, mr MsgAwaitingConfirmation)
    ClientConfirmed    -> (mr MsgDepositConfirmed, mr MsgAwaitingExecution)
    ClientCancelled t  -> (mr MsgUserCancelled, mempty)
    OperatorAccepted _ -> (mr MsgDepositExecuted, mempty)
    OperatorRejected _ -> (mr MsgDepositRejected, mempty)

genericRequestAmount :: (Int, Bool, Int, Currency) -> Widget -> Widget
genericRequestAmount (a, rejected, f, c) d =
    [whamlet|
        <big>
            $if rejected
                <s>#{ac}#
            $else
                <b>#{ac}#
            <small .text-muted>
                #{currSign c}
        <br>
        <small .text-muted>
            #{sign}#{cents2dblT f}#
            <small>
                #{currSign c}
            ^{d}
        |]
  where
    sign = if f == 0 then "" else "-" :: String
    ac = cents2dblT a

genericRequestStatus :: (Widget, Widget, Widget) -> Widget
genericRequestStatus (extra, status, description) =
    [whamlet|
        ^{extra}
        <div>
            <span .text-uppercase>
                ^{status}
            ^{description}
        |]


isNew :: DepositRequest -> Bool
isNew r = depositRequestStatus r == New

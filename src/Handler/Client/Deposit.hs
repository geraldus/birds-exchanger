{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Handler.Client.Deposit where

import           Import                 hiding ( on, (==.) )

import           Form.Profile.Deposit
import           Local.Persist.Currency ( Currency (..), currSign )
import           Local.Persist.Wallet   ( DepositRequestStatus (..) )
import           Utils.I18n

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
        (genericRequestAmount (requestAmounts d) (desc d))
        (genericRequestStatus (requestStatuses ur mr (fd, ft) d))
  where
    desc d = case d of
        NoDetails _ -> mempty
        _           -> [whamlet|\ (_{MsgInFact})|]
    isRejected (RejectD _ _) = True
    isRejected _             = False

genericRow :: Entity DepositRequest -> Widget -> Widget -> Widget
genericRow (Entity ident r@DepositRequest{..}) expected status =
    toWidget [whamlet|
        <tr .data-row #data-row-#{fromSqlKey ident}>
            <td>^{requestTimeW r}
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
        |]

requestTimeW :: DepositRequest -> Widget
requestTimeW DepositRequest{..} =
    toWidget [whamlet|
        <small .text-muted>
            ^{dateTimeRowW depositRequestCreated}|]

requestAmounts :: Details -> (Int, Bool, Int, Currency)
requestAmounts (NoDetails (Entity _ DepositRequest{..})) =
    let amt = depositRequestCentsAmount - depositRequestExpectedFeeCents
    in (amt, False, depositRequestExpectedFeeCents, depositRequestCurrency)
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
    let (_, dm) = requestStatusMessages mr r
        code = depositRequestTransactionCode
        extra = [whamlet|
            <a href=#{ur (DepositRequestConfirmationR code)}>
                #{mr MsgDepositConfirmTransfer}|]
        desc = [whamlet|<i .text-muted><small>#{dm}|]
    in (extra, mempty, desc)
requestStatuses
    _ mr (fd, ft) (AcceptD (Entity _ r) (Entity _ AcceptedDeposit{..})) =
        let (sm, _) = requestStatusMessages mr r
            status = [whamlet|#{sm}<br>|]
            desc = [whamlet|<small .text-muted>
                #{fd acceptedDepositAccepted} #{ft acceptedDepositAccepted}|]
        in (mempty, status, desc)
requestStatuses
    _ mr (fd, ft) (RejectD (Entity _ r) (Entity _ DepositReject{..})) =
        let (sm, _) = requestStatusMessages mr r
            status = [whamlet|#{sm}<br>|]
            desc = [whamlet|<small .text-muted>
                #{fd depositRejectTime} #{ft depositRejectTime}
                <br>
                #{depositRejectReason}|]
        in (mempty, status, desc)

requestStatusMessages :: (AppMessage -> Text) -> DepositRequest -> (Text, Text)
requestStatusMessages mr r = case depositRequestStatus r of
    New                -> (mempty, mr MsgAwaitingConfirmation)
    ClientConfirmed    -> (mr MsgDepositConfirmed, mr MsgAwaitingExecution)
    OperatorAccepted _ -> (mr MsgDepositExecuted, mempty)
    OperatorRejected _ -> (mr MsgDepositRejected, mempty)

genericRequestAmount :: (Int, Bool, Int, Currency) -> Widget -> Widget
genericRequestAmount (a, rejected, f, c) desc =
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
            ^{desc}
        |]
  where
    sign = if f == 0 then "" else "-" :: String
    ac = cents2dblT a

genericRequestStatus :: (Widget, Widget, Widget) -> Widget
genericRequestStatus (extra, status, desc) =
    [whamlet|
        ^{extra}
        <p>
            <span .text-uppercase>^{status}
            ^{desc}
        |]


getRenders :: WidgetFor App (Route App -> Text, AppMessage -> Text)
getRenders = (,) <$> liftHandler getUrlRender <*> liftHandler getMessageRender

dateTimeRowW :: UTCTime -> Widget
dateTimeRowW t = do
    fd <- getFormatDateRender
    ft <- getFormatTimeRender
    [whamlet|#{ft t} #{fd t}|]

getFormatDateRender :: WidgetFor App (UTCTime -> Html)
getFormatDateRender = (\(l, t) -> localeFormatDate l . offsetTime t)
    <$> getFormatParams

getFormatTimeRender :: WidgetFor App (UTCTime -> Html)
getFormatTimeRender = (\(l, t) -> localeFormatTime l . offsetTime t)
    <$> getFormatParams

getFormatParams :: WidgetFor App (TimeLocale, Int)
getFormatParams = (,)
    <$> liftHandler selectLocale
    <*> liftHandler timezoneOffsetFromCookie

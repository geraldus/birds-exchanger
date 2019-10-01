{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Handler.Client.Deposit where

import           Import                 as I hiding ( on, (==.) )

import           Form.Profile.Deposit
import           Local.Persist.Currency ( Currency (..), currSign )
import           Local.Persist.Wallet   ( DepositRequestStatus (..) )
import           Type.App
import           Utils.App.Client
import           Utils.App.Common
import           Utils.Common
import           Utils.I18n
import           Utils.Money
import           Utils.Time             ( renderDateRow,
                                          timezoneOffsetFromCookie )

import           Data.Aeson             ( encode )
import           Data.Time.Clock        ( addUTCTime, secondsToDiffTime )
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
        FormMissing -> defaultLayout $
                defaultWidget formId widget enctype mayError
        FormFailure _ -> defaultLayout $
                defaultWidget formId widget enctype mayError
        FormSuccess DepositRequestFD{..} -> do
            code <- appNonce128urlT
            time <- liftIO getCurrentTime
            paymentAddressee <- getNextPaymentAddressee
                defaultSelectNextAddr depReqTransferMethod
            render <- getMessageRender
            case paymentAddressee of
                Nothing -> defaultLayout $
                    defaultWidget
                        formId
                        widget
                        enctype
                        (Just [ render MsgNoPaymentMethodAvailable ])
                Just (PaymentAddress depReqPaymentAddressee _) -> do
                    let depReqRecord = DepositRequest
                            depReqCurrency
                            depReqTransferMethod
                            (decodeUtf8 . toStrict . encode $
                                    depReqPaymentAddressee)
                            depReqCentsAmount
                            depReqCentsExpectedFee
                            code
                            depReqTargetCurrency
                            1
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
    $(widgetFile "client/request/deposit/index")


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
    locale <- selectLocale
    tzoffset <- timezoneOffsetFromCookie
    depositDetails <- liftHandler . runDB . select $
        from $ \(r `LeftOuterJoin` macc `LeftOuterJoin` mrej) -> do
            on (just (r ^. DepositRequestId) ==. mrej ?. DepositRejectRequestId)
            on (just (r ^. DepositRequestId) ==. macc ?. AcceptedDepositDepositRequestId)
            where_ (r ^. DepositRequestUserId ==. val clientId)
            orderBy [desc (r ^. DepositRequestCreated)]
            return (r, macc, mrej)
    let list = map wrapDetails depositDetails
    let dateGroups = groupByDate depositDetails tzoffset
    let body = concatMap depositHistoryRow list
    $(widgetFile "client/request/deposit/mobile/list")
    $(widgetFile "client/request/deposit/desktop/table")


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
    $(widgetFile "client/request/deposit/desktop/row")

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

mobileDepositListItem :: DepositDetails -> Widget
mobileDepositListItem details@(Entity did deposit@DepositRequest{..}, _, _) = do
    render <- getMessageRender
    renderUrl <- getUrlRender
    let statusMessages = requestStatusMessages render deposit
        status = if null (fst statusMessages)
            then snd statusMessages
            else fst statusMessages
        selectAction = selectAction' renderUrl render
    let depositAction = selectAction $ wrapDetails details
    $(widgetFile "client/request/deposit/mobile/item")
    where
        selectAction' ur mr (NoDetails _) =
            case depositRequestStatus of
                New -> [whamlet|
                        <a href=#{url}>
                            #{mr MsgDepositConfirmTransfer}|]
                _ -> mempty
            where
                url = ur $
                    DepositRequestConfirmationR depositRequestTransactionCode
        selectAction' _ _ _ = mempty


isNew :: DepositRequest -> Bool
isNew r = depositRequestStatus r == New

groupByDate :: [ DepositDetails ] -> Int -> [(UTCTime, [DepositDetails])]
groupByDate gs tzoffset = foldr labelGroup [] grouped
    where
        labelGroup gs' acc = case gs of
            (g, _, _):_ -> acc ++ [(depositRequestCreated $ entityVal g, gs')]
            _ -> error . concat $
                    [ "This shouldn't happen, "
                    , "deposit date group must hava at least 1 element"
                    ]

        grouped = flip I.groupBy gs $ \(g1, _, _) (g2, _, _) ->
            utcTzo g1 == utcTzo g2

        utcTzo =
            utctDay . addUTCTime tzoTime . depositRequestCreated . entityVal

        tzoTime = fromRational . toRational . secondsToDiffTime $
            fromIntegral tzoffset

wrapDetails :: DepositDetails -> Details
wrapDetails (r, Just accepted, _) = AcceptD r accepted
wrapDetails (r, _, Just rejected) = RejectD r rejected
wrapDetails (r, _, _)             = NoDetails r

unDetailsRequest :: Details -> Entity DepositRequest
unDetailsRequest (NoDetails r) = r
unDetailsRequest (AcceptD r _) = r
unDetailsRequest (RejectD r _) = r

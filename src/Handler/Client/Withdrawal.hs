{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Handler.Client.Withdrawal where

import           Import                  as I hiding ( on, (==.) )

import           Form.Profile.Withdrawal
import           Local.Persist.Currency  ( Currency (..), currencySymbol )
import           Local.Persist.Wallet
import           Type.Money              ( Money (..) )
import           Type.Withdrawal
import           Utils.App.Client
import           Utils.App.Common
import           Utils.Common
import           Utils.I18n
import           Utils.Money
import           Utils.Time              ( renderDateRow,
                                           timezoneOffsetFromCookie,
                                           utcDayWithTimeZoneAdded )

import           Database.Esqueleto
import qualified Database.Esqueleto      as Esqueleto
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
                            WsNew
                            Nothing
                    let transaction = WalletBalanceTransaction
                            wid
                            (BalanceWithdrawal (negate amount2Freeze))
                            reasonId
                            walletCents
                            time
                            WithdrawalCreation
                    (recId, _) <- runDB $ do
                        r <- insert record
                        t <- insert transaction
                        I.update wid [UserWalletAmountCents I.-=. amount2Freeze]
                        return (r, t)
                    notify' (Entity recId record)
                    setMessage "Заявка на вывод успешно создана"
                    redirect WithdrawalR
  where
    notify' r = do
        ch <- appChannelsOperatorWithdrawalCreate . appChannels <$> getYesod
        liftIO . atomically $ writeTChan ch r

defaultWidget :: Text -> Widget -> Enctype -> Maybe [Text] -> Widget
defaultWidget formId widget enctype mayError = do
    setAppPageTitle MsgClientWithdrawalPageTitle
    messageRender <- liftHandler getMessageRender
        :: WidgetFor App (AppMessage -> Text)
    $(widgetFile "client/request/common")
    $(widgetFile "client/request/withdrawal/index")

type WithdrawalDetails =
    ( Entity WithdrawalRequest
    , Esqueleto.Value Currency
    , Maybe (Entity WithdrawalCancel)
    , Maybe (Entity WithdrawalAccept)
    , Maybe (Entity WithdrawalReject)
    )

data Details
    = NoDetails (Entity WithdrawalRequest) Currency
    | AcceptD (Entity WithdrawalRequest) Currency (Entity WithdrawalAccept)
    | RejectD (Entity WithdrawalRequest) Currency (Entity WithdrawalReject)
    | CancelD (Entity WithdrawalRequest) Currency (Entity WithdrawalCancel)
    deriving Show

withdrawalHistory :: Widget
withdrawalHistory = do
    clientId <- liftHandler requireClientId
    locale <- selectLocale
    tzoffset <- timezoneOffsetFromCookie
    withdrawalDetails <- liftHandler . runDB . select $
        from $ \(u `InnerJoin` w `InnerJoin` r `LeftOuterJoin` mcan `LeftOuterJoin` macc `LeftOuterJoin` mrej) -> do
            on (just (r ^. WithdrawalRequestId) ==. mrej ?. WithdrawalRejectRequestId)
            on (just (r ^. WithdrawalRequestId) ==. macc ?. WithdrawalAcceptRequestId)
            on (just (r ^. WithdrawalRequestId) ==. mcan ?. WithdrawalCancelRequestId)
            on (w ^. UserWalletId ==. r ^. WithdrawalRequestWalletId)
            on (u ^. UserId ==. w ^. UserWalletUserId)
            where_ (u ^. UserId ==. val clientId)
            orderBy [desc (r ^. WithdrawalRequestCreated)]
            return (r, w ^. UserWalletCurrency, mcan, macc, mrej)
    let list = map wrapDetails withdrawalDetails
    let dateGroups = reverse (groupByDate withdrawalDetails tzoffset)
    let body = concatMap withdrawalHistoryRow list
    $(widgetFile "client/request/withdrawal/desktop/table")
    $(widgetFile "client/request/withdrawal/mobile/list")


unDetailsRequest :: Details -> Entity WithdrawalRequest
unDetailsRequest (NoDetails r _) = r
unDetailsRequest (AcceptD r _ _) = r
unDetailsRequest (RejectD r _ _) = r
unDetailsRequest (CancelD r _ _) = r

unDetailsCurrency :: Details -> Currency
unDetailsCurrency (NoDetails _ c) = c
unDetailsCurrency (AcceptD _ c _) = c
unDetailsCurrency (RejectD _ c _) = c
unDetailsCurrency (CancelD _ c _) = c

withdrawalHistoryRow :: Details -> Widget
withdrawalHistoryRow d = do
    let r = unDetailsRequest d
        c = unDetailsCurrency d
    (ur, mr) <- getRenders
    fd <- getFormatDateRender
    ft <- getFormatTimeRender
    genericRow
        r
        c
        (isCancelledOrRejected d)
        (genericRequestAmount (requestAmounts d) c (description d))
        (genericRequestStatus (requestStatuses ur mr (fd, ft) d))
  where
    description dsc = case dsc of
        NoDetails _ _ -> mempty
        _             -> [whamlet|\ (_{MsgInFact})|]

genericRow
    :: Entity WithdrawalRequest
    -> Currency
    -> Bool
    -- ^ 'True' if request was cancelled or rejected
    -> Widget
    -> Widget
    -> Widget
genericRow (Entity ident r@WithdrawalRequest{..}) c strikeout expected status =
    $(widgetFile "client/request/withdrawal/desktop/row")
  where
    valueW :: Widget
    valueW = [whamlet|
        #{cents2dblT withdrawalRequestCentsAmount}#
        <small .text-muted>
            #{currencySymbol c}
        |]

genericRequestAmount :: (Int, Bool) -> Currency -> Widget -> Widget
genericRequestAmount (a, _) c d =
    [whamlet|
        #{sign}#{ac}#
        <small .text-muted>
            #{currencySymbol c}
        <small .text-muted>
            ^{d}
        |]
  where
    sign = if a == 0 then "" else "-" :: String
    ac = cents2dblT a

genericRequestStatus :: (Widget, Widget) -> Widget
genericRequestStatus (status, description) =
    [whamlet|
        <p>
            <span .text-uppercase>
                ^{status}
            ^{description}
        |]

requestAmounts :: Details -> (Int, Bool)
requestAmounts (NoDetails (Entity _ WithdrawalRequest{..}) _) =
    (withdrawalRequestFeeAmount, False)
requestAmounts (AcceptD _ _ (Entity _ WithdrawalAccept{..})) =
    (withdrawalAcceptActualFee, False)
requestAmounts RejectD{} = (0, True)
requestAmounts CancelD{} = (0, True)

requestStatuses
    :: (Route App -> Text)
    -> (AppMessage -> Text)
    -> (UTCTime -> Html, UTCTime -> Html)
    -> Details
    -> (Widget, Widget)
requestStatuses _ mr _ (NoDetails (Entity _ WithdrawalRequest{..}) _) =
    let status = [whamlet|#{mr MsgRequestProcessing}<br>|]
    in (status, mempty)
requestStatuses
    _ mr (fd, ft) (CancelD _ _ (Entity _ WithdrawalCancel{..})) =
        let status = [whamlet|#{mr MsgUserCancelled}<br>|]
            description = [whamlet|<small .text-muted>
                #{fd withdrawalCancelTime} #{ft withdrawalCancelTime} |]
        in (status, description)
requestStatuses
    _ mr (fd, ft) (AcceptD _ _ (Entity _ WithdrawalAccept{..})) =
        let status = [whamlet|#{mr MsgDepositExecuted}<br>|]
            description = [whamlet|<small .text-muted>
                #{fd withdrawalAcceptTime} #{ft withdrawalAcceptTime}|]
        in (status, description)
requestStatuses
    _ mr (fd, ft) (RejectD _ _ (Entity _ WithdrawalReject{..})) =
        let status = [whamlet|#{mr MsgDepositRejected}<br>|]
            description = [whamlet|<small .text-muted>
                #{fd withdrawalRejectTime} #{ft withdrawalRejectTime}
                <br>
                #{withdrawalRejectReason}|]
        in (status, description)

mobileWithdrawalListItem :: WithdrawalDetails -> Widget
mobileWithdrawalListItem
        details@(Entity wid _w@WithdrawalRequest{..}, c, _, _, _) = do
    render <- getMessageRender
    renderUrl <- getUrlRender
    fd <- getFormatDateRender
    ft <- getFormatTimeRender
    let wrappedDetails = wrapDetails details
    let statusMessages =
                requestStatuses renderUrl render (fd, ft) wrappedDetails
        status = fst statusMessages
    let action = mempty :: Html -- selectAction $ wrapDetails details
    let withdrawalCurrency = unValue c
    $(widgetFile "client/request/withdrawal/mobile/item")
    where


isNew :: WithdrawalRequest -> Bool
isNew r = withdrawalRequestStatus r == WsNew

isCancelledOrRejected :: Details -> Bool
isCancelledOrRejected CancelD{} = True
isCancelledOrRejected RejectD{} = True
isCancelledOrRejected _         = False

groupByDate :: [ WithdrawalDetails ] -> Int -> [(UTCTime, [WithdrawalDetails])]
groupByDate gs tzoffset = foldr labelGroup [] grouped
    where
        labelGroup gs' acc = case gs' of
            (g, _, _, _, _):_ ->
                    acc
                    <> [(withdrawalRequestCreated $ entityVal g, gs')]
            _ -> error . concat $
                    [ "This shouldn't happen, "
                    , "withdrawal date group must have at least 1 element"
                    ]

        grouped = flip I.groupBy gs $ \(g1, _, _, _, _) (g2, _, _, _, _) ->
            utcTzoDay g1 == utcTzoDay g2

        utcTzoDay = utcDayWithTimeZoneAdded tzoTime withdrawalRequestCreated

        tzoTime = fromIntegral tzoffset

wrapDetails :: WithdrawalDetails -> Details
wrapDetails (r, c, Just cancelled, _, _) = CancelD r (unValue c) cancelled
wrapDetails (r, c, _, Just accepted, _)  = AcceptD r (unValue c) accepted
wrapDetails (r, c, _, _, Just rejected)  = RejectD r (unValue c) rejected
wrapDetails (r, c, _, _, _)              = NoDetails r (unValue c)

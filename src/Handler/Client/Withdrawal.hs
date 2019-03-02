{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Handler.Client.Withdrawal where

import           Import

import           Form.Profile.Withdrawal
import           Local.Persist.Currency  ( Currency (..), currSign )
import           Local.Persist.Wallet
import           Type.Money              ( Money (..) )
import           Type.Withdrawal
import           Utils.App.Client
import           Utils.App.Common
import           Utils.I18n
import           Utils.Money

import qualified Database.Esqueleto      as E
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
    <form
            ##{formId}
            method=post
            enctype=#{enctype}
            action=@{WithdrawalCreateR}
            .col-12 .col-sm-10 .col-md-8
            .mx-auto>
        ^{widget}
        <div .form-group .row .justify-content-center>
            <button .btn.btn-lg.btn-outline-primary .mt-2 type=submit>вывод
    ^{withdrawalHistory}
    |]


type WithdrawalDetails =
    ( Entity WithdrawalRequest
    , Maybe (Entity WithdrawalAccept)
    , Maybe (Entity WithdrawalReject) )

data Details
    = NoDetails (Entity WithdrawalRequest) Currency
    | AcceptD (Entity WithdrawalRequest) Currency (Entity WithdrawalAccept)
    | RejectD (Entity WithdrawalRequest) Currency (Entity WithdrawalReject)
    deriving Show


withdrawalHistory :: Widget
withdrawalHistory = do
    clientId <- liftHandler requireClientId
    withdrawalDetails <- liftHandler . runDB . E.select $
        E.from $ \(u `E.InnerJoin` w `E.InnerJoin` r `E.LeftOuterJoin` macc `E.LeftOuterJoin` mrej) -> do
            E.on (E.just (r E.^. WithdrawalRequestId) E.==. mrej E.?. WithdrawalRejectRequestId)
            E.on (E.just (r E.^. WithdrawalRequestId) E.==. macc E.?. WithdrawalAcceptRequestId)
            E.on (w E.^. UserWalletId E.==. r E.^. WithdrawalRequestWalletId)
            E.on (u E.^. UserId E.==. w E.^. UserWalletUserId)
            E.where_ (u E.^. UserId E.==. E.val clientId)
            E.orderBy [E.desc (r E.^. WithdrawalRequestCreated)]
            return (r, w E.^. UserWalletCurrency, macc, mrej)
    let list = map wrapDetails withdrawalDetails
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
                $forall i <- list
                    ^{withdrawalHistoryRow' i}
        |]
  where
    wrapDetails (r, c, Just accepted, _) = AcceptD r (E.unValue c) accepted
    wrapDetails (r, c, _, Just rejected) = RejectD r (E.unValue c) rejected
    wrapDetails (r, c, _, _)             = NoDetails r (E.unValue c)

unDetailsRequest :: Details -> Entity WithdrawalRequest
unDetailsRequest (NoDetails r _) = r
unDetailsRequest (AcceptD r _ _) = r
unDetailsRequest (RejectD r _ _) = r

unDetailsCurrency :: Details -> Currency
unDetailsCurrency (NoDetails _ c) = c
unDetailsCurrency (AcceptD _ c _) = c
unDetailsCurrency (RejectD _ c _) = c

withdrawalHistoryRow' :: Details -> Widget
withdrawalHistoryRow' d = do
    let r = unDetailsRequest d
        c = unDetailsCurrency d
    (ur, mr) <- getRenders
    fd <- getFormatDateRender
    ft <- getFormatTimeRender
    genericRow
        r
        c
        (genericRequestAmount (requestAmounts d) c (description d))
        (genericRequestStatus (requestStatuses ur mr (fd, ft) d))
  where
    description d = case d of
        NoDetails _ _ -> mempty
        _             -> [whamlet|\ (_{MsgInFact})|]

genericRow :: Entity WithdrawalRequest -> Currency -> Widget -> Widget -> Widget
genericRow (Entity ident r@WithdrawalRequest{..}) c expected status =
    toWidget [whamlet|
        <tr .data-row #data-row-#{fromSqlKey ident}>
            <td>^{requestTimeW withdrawalRequestCreated}
            <td .align-middle>
                #{cents2dblT withdrawalRequestCentsAmount}#
                <small .text-muted>
                    #{currSign c}
                <br>
                <small .text-muted>
                    _{transferMethodMsg withdrawalRequestMethod}
            <td .align-middle>
                ^{expected}
            <td .align-middle>
                ^{status}|]

genericRequestAmount :: (Int, Bool) -> Currency -> Widget -> Widget
genericRequestAmount (a, rejected) c d =
    [whamlet|
        #{sign}#{ac}#
        <small .text-muted>
            #{currSign c}
        <small .text-muted>^{d}
        |]
  where
    sign = if a == 0 then "" else "-" :: String
    ac = cents2dblT a

genericRequestStatus :: (Widget, Widget) -> Widget
genericRequestStatus (status, description) =
    [whamlet|
        <p>
            <span .text-uppercase>^{status}
            ^{description}
        |]

requestAmounts :: Details -> (Int, Bool)
requestAmounts (NoDetails (Entity _ WithdrawalRequest{..}) _) =
    (withdrawalRequestFeeAmount, False)
requestAmounts (AcceptD (Entity _ r) _ (Entity _ WithdrawalAccept{..})) =
    (withdrawalAcceptActualFee, False)
requestAmounts (RejectD (Entity _ r) _ _) =
    (0, True)

requestStatuses
    :: (Route App -> Text)
    -> (AppMessage -> Text)
    -> (UTCTime -> Html, UTCTime -> Html)
    -> Details
    -> (Widget, Widget)
requestStatuses ur mr fs (NoDetails (Entity _ r@WithdrawalRequest{..}) _) =
    let status = [whamlet|#{mr MsgRequestProcessing}<br>|]
    in (status, mempty)
requestStatuses
    _ mr (fd, ft) (AcceptD (Entity _ r) _ (Entity _ WithdrawalAccept{..})) =
        let status = [whamlet|#{mr MsgDepositExecuted}<br>|]
            description = [whamlet|<small .text-muted>
                #{fd withdrawalAcceptTime} #{ft withdrawalAcceptTime}|]
        in (status, description)
requestStatuses
    _ mr (fd, ft) (RejectD (Entity _ r) _ (Entity _ WithdrawalReject{..})) =
        let status = [whamlet|#{mr MsgDepositRejected}<br>|]
            description = [whamlet|<small .text-muted>
                #{fd withdrawalRejectTime} #{ft withdrawalRejectTime}
                <br>
                #{withdrawalRejectReason}|]
        in (status, description)

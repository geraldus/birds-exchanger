{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module Handler.Operator.DepositRequestsList where


import           Import
import           Local.Persist.Currency ( currSign, tmTShort, rurC, pzmC )
import           Local.Persist.Deposit
import           Utils.Deposit
import           Utils.Money            ( truncCoins2Cents )
import Type.Fee

import           Data.Time.Format       ( TimeLocale, formatTime )
import           Database.Persist.Sql   ( fromSqlKey, rawSql )

getOperatorDepositRequestsListR :: Handler Html
getOperatorDepositRequestsListR = do
    requireStaffId
    loc <- selectLocale
    let reqDateT = renderRequestTime loc
    -- let rurFee = selectFee rurC
    --     pzmFee = selectFee pzmC
    list <-
        runDB $ rawSql s [toPersistValue ClientConfirmed] :: Handler
            [(Entity DepositRequest, Entity UserWallet, Entity User)]
        -- selectList [DepositRequestStatus ==. ClientConfirmed] []
    defaultLayout $(widgetFile "operator/deposit-requests-list")
  where
    s
        = "SELECT ??, ??, ?? FROM deposit_request, user_wallet, \"user\" \
        \ WHERE deposit_request.status = ? \
        \ AND deposit_request.archived = FALSE \
        \ AND deposit_request.user_id = \"user\".id \
        \ AND user_wallet.currency = deposit_request.currency \
        \ AND user_wallet.user_id = deposit_request.user_id \
        \ ORDER BY deposit_request.created ASC"


renderSums :: DepositRequest -> Html
renderSums req@DepositRequest{..} =
    let iC = currSign depositRequestCurrency
        tC = currSign depositRequestTargetCurrency
        ratio = selectRatio depositRequestCurrency depositRequestTargetCurrency
        ratioT = renderRequestRatio req
        reqAmt = depositRequestCentsAmount
        reqAmtT = cents2dblT reqAmt
        feeAmt = calcFeeCents (selectDepositFee depositRequestCurrency) reqAmt
        feeAmtT = cents2dblT feeAmt
        depAmt = convertCents ratio (reqAmt - feeAmt)
        depAmtT = cents2dblT depAmt
    in [shamlet|
            <b>#{reqAmtT}&nbsp;#{iC} #
            <small>(-#{feeAmtT}&nbsp;#{iC})
            <br>
            <small>
                #{depAmtT}&nbsp;#{tC} #
                <small>(x#{ratioT})
            |]

renderRequestRatio :: DepositRequest -> Html
renderRequestRatio DepositRequest{..} =
    let ratio = selectRatio depositRequestCurrency depositRequestTargetCurrency
        ratioT = cents2dblT . truncCoins2Cents $ ratio
    in [shamlet|#{ratioT}|]


renderMethodUser :: DepositRequest -> Entity User -> Html
renderMethodUser req (Entity userId user) = [shamlet|
    #{tmTShort (depositRequestTransferMethod req)}
    <br>
    <small>
        <a .user-profile-link href="management/user-view/#{fromSqlKey userId}">
            #{userIdent user}|]

renderRequestTime :: TimeLocale -> DepositRequest -> Html
renderRequestTime loc req = [shamlet|
    #{timeFT utc}
    <br>
    <small>
        #{dateFT utc}
    |]
  where
    utc = depositRequestCreated req
    timeFT t = toHtml . formatTime loc ("%H:%M:%S" :: String) $ (t :: UTCTime)
    dateFT t = toHtml . formatTime loc ("%d.%m.%Y" :: String) $ (t :: UTCTime)

renderRequestExpectedTotal :: DepositRequest -> Html
renderRequestExpectedTotal DepositRequest{..} = [shamlet|#{cents2dblT total}|]
  where
    total = convertCents depositRequestExpectedConversionRatio (depositRequestCentsAmount - depositRequestExpectedFeeCents)

renderFeeAsPct :: DepositRequest -> Html
renderFeeAsPct DepositRequest{..} = [shamlet|#{ren}|]
  where
    fee = selectDepositFee depositRequestCurrency
    ren = case fee of
            CentsFixed _ -> error "no fixed fee logics"
            Percent p -> show p

renderFeeAsDbl :: DepositRequest -> Html
renderFeeAsDbl DepositRequest{..} = [shamlet|#{ren}|]
  where
    fee = selectDepositFee depositRequestCurrency
    ren = case fee of
            CentsFixed _ -> error "no fixed fee logics"
            Percent p -> show $ p / 100
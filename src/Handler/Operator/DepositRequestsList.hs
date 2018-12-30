{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Handler.Operator.DepositRequestsList where


import           Import
import           Local.Persist.Currency ( currSign, tmTShort )
import           Local.Persist.Deposit
import           Utils.Deposit
import           Utils.Money            ( truncCoins2Cents )

import           Database.Persist.Sql   ( fromSqlKey, rawSql )


getOperatorDepositRequestsListR :: Handler Html
getOperatorDepositRequestsListR = do
    _    <- requireStaffId
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
renderSums DepositRequest{..} =
    let iC = currSign depositRequestCurrency
        tC = currSign depositRequestTargetCurrency
        ratio = selectRatio depositRequestCurrency depositRequestTargetCurrency
        ratioT = cents2dblT . truncCoins2Cents $ ratio
        reqAmt = depositRequestCentsAmount
        reqAmtT = cents2dblT reqAmt
        feeAmt = calcFeeCents (selectFee depositRequestCurrency) reqAmt
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

renderMethodUser :: DepositRequest -> Entity User -> Html
renderMethodUser req (Entity userId user) = [shamlet|
    #{tmTShort (depositRequestTransferMethod req)}
    <br>
    <small>
        <a href="management/user-view/#{fromSqlKey userId}">
            #{userIdent user}|]

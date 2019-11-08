{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module Handler.Operator.DepositRequestsList where

import           Import                 as I hiding ( on, (==.) )
import           Local.Persist.Currency ( currSign, tmTShort )
import           Local.Persist.Wallet   ( DepositRequestStatus (..) )
import           Utils.Common           ( selectLocale )
import           Utils.Deposit
import           Utils.Money
import           Utils.Render
import           Utils.Time

import           Database.Esqueleto     as E
import           Database.Persist.Sql   ( fromSqlKey )


getOperatorDepositRequestsListR :: Handler Html
getOperatorDepositRequestsListR = do
    requireOperatorId
    loc <- selectLocale
    tzo <- timezoneOffsetFromCookie
    let reqDateT = renderTimeDateCol loc tzo . depositRequestCreated
    renderUrl <- getUrlRender
    list <- runDB selectData
    let reactBuild =
#ifdef DEVELOPMENT
            "development"
#else
            "production.min"
#endif
    defaultLayout $ do
        addScriptRemote $ "https://unpkg.com/react@16/umd/react." <> reactBuild <> ".js"
        addScriptRemote $ "https://unpkg.com/react-dom@16/umd/react-dom." <> reactBuild <> ".js"
        $(widgetFile "operator/common")
        $(widgetFile "operator/request-list-common")
        $(widgetFile "operator/deposit-requests-list")
        addScriptAttrs (StaticR js_bundle_js) []
  where
    selectData = select $ from $ \(depReq `LeftOuterJoin` addr, wlt, usr) -> do
        on (addr ?. DepositPayerDeposit ==. just (depReq ^. DepositRequestId))
        where_ (
            (depReq ^. DepositRequestStatus  ==. val ClientConfirmed)
            &&. (depReq ^. DepositRequestArchived ==. val False)
            &&. (depReq ^. DepositRequestUserId  ==. usr ^. UserId)
            &&. (depReq ^. DepositRequestCurrency ==. wlt ^. UserWalletCurrency)
            &&. (wlt ^. UserWalletUserId ==. usr ^. UserId) )
        orderBy [ asc (depReq ^. DepositRequestCreated) ]
        return (depReq, wlt, usr, addr)

renderSums :: DepositRequest -> Html
renderSums req@DepositRequest{..} =
    let iC = currSign depositRequestCurrency
        tC = currSign depositRequestTargetCurrency
        ratio = 1
        ratioT = renderRequestRatio req
        reqAmt = depositRequestCentsAmount
        reqAmtT = cents2dblT reqAmt
        feeAmt = calcFeeCents (selectDepositFee depositRequestCurrency) reqAmt
        feeAmtT = cents2dblT feeAmt
        depAmt = multiplyCents ratio (reqAmt - feeAmt)
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
    let ratio = 1 -- selectRatio depositRequestCurrency depositRequestTargetCurrency
        ratioT = cents2dblT . truncCoins2Cents $ ratio
    in [shamlet|#{ratioT}|]


renderMethodUser :: DepositRequest -> Entity User -> Html
renderMethodUser req (Entity userId user) = [shamlet|
    #{tmTShort (depositRequestTransferMethod req)}
    <br>
    <small>
        <a
                .user-profile-link
                target=_blank
                href="/operator/user-history/#{fromSqlKey userId}">
            #{userIdent user}|]

renderRequestExpectedTotal :: DepositRequest -> Html
renderRequestExpectedTotal DepositRequest{..} = [shamlet|#{cents2dblT total}|]
  where
    total =
        multiplyCents
            depositRequestExpectedConversionRatio
            (depositRequestCentsAmount - depositRequestExpectedFeeCents)

renderReqFeeAsPct :: DepositRequest -> Html
renderReqFeeAsPct DepositRequest{..} = renderFeeAsPct fee
  where
    fee = selectDepositFee depositRequestCurrency

renderReqFeeAsDbl :: DepositRequest -> Html
renderReqFeeAsDbl DepositRequest{..} = renderFeeAsDbl fee
  where
    fee = selectDepositFee depositRequestCurrency

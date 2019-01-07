{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Handler.Operator.WithdrawalRequest where


import           Import
import           Local.Persist.Currency
import           Local.Persist.ExchangeOrder    ( ProfitType(..) )
import           Utils.Render                   ( renderFeeAsPct )
import           Utils.Time                     ( renderTimeDateRu )
import           Utils.Withdrawal

import qualified Data.Text                     as T
import           Database.Persist.Sql           ( fromSqlKey
                                                , rawSql
                                                , toSqlKey
                                                )
import           Text.Blaze.Html                ( preEscapedToHtml )


getOperatorWithdrawalRequestsListR :: Handler Html
getOperatorWithdrawalRequestsListR = do
    requireStaffId
    loc <- selectLocale
    let reqDateT = renderTimeDateRu loc . withdrawalRequestCreated
    list <-
        runDB $ rawSql s [] :: Handler
            [(Entity WithdrawalRequest, Entity UserWallet, Entity User)]
    defaultLayout $ do
        $(widgetFile "operator/request-list-common")
        $(widgetFile "operator/withdrawal-requests-list")
  where
    s
        = "SELECT ??, ??, ?? FROM withdrawal_request, user_wallet, \"user\" \
        \ WHERE withdrawal_request.accepted IS NULL \
        \ AND withdrawal_request.wallet_id = user_wallet.id \
        \ AND user_wallet.user_id = \"user\".id \
        \ ORDER BY withdrawal_request.created ASC"


postOperatorAcceptWithdrawalRequestR :: Handler Html
postOperatorAcceptWithdrawalRequestR = do
    staffId <- requireStaffId
    let mStaffUserId = case staffId of
            Left uid -> Just uid
            _        -> Nothing
    wid <- toSqlKey <$> runInputPost (ireq intField "withdrawal-id")
    request <- runDB $ get404 wid
    wallet <- runDB $ get404 (withdrawalRequestWalletId request)
    fee <- runInputPost (ireq intField "withdrawal-fee")
    trans <- runInputPost (ireq intField "withdrawal-transfered")
    time <- liftIO getCurrentTime
    runDB $ do
        insert $ WithdrawalAccept wid
                                  trans
                                  fee
                                  (pack $ show staffId)
                                  mStaffUserId
        update wid [WithdrawalRequestAccepted =. Just time]
        insert $ InnerProfitRecord
            (withdrawalRequestWalletTransactionReasonId request)
            (userWalletCurrency wallet)
            fee
            WithdrawalFee
    redirect OperatorWithdrawalRequestsListR

renderReqAddress :: UserWallet -> WithdrawalRequest -> Html
renderReqAddress w r
    | userWalletCurrency w == CryptoC PZM = [shamlet|
        <small>
            <small>
                #{adr}
        |]
    | otherwise = adr
  where
    adr = renderAddress (withdrawalRequestAddressee r)

renderAddress :: Text -> Html
renderAddress adr = preEscapedToHtml res
  where
    adrs = T.lines adr
    res  = T.intercalate "<br>" adrs

renderWalletCurrentFee :: UserWallet -> Html
renderWalletCurrentFee w = renderFeeAsPct fee
  where
    currency = userWalletCurrency w
    fee      = selectWithdrawalFee currency

renderReqFee :: UserWallet -> WithdrawalRequest -> Html
renderReqFee w r = [shamlet|#{cents2dblT (reqFeeCents w r)}|]

renderReqFeeCents :: UserWallet -> WithdrawalRequest -> Html
renderReqFeeCents w r = [shamlet|#{show (reqFeeCents w r)}|]

reqFeeCents :: UserWallet -> WithdrawalRequest -> Int
reqFeeCents UserWallet {..} WithdrawalRequest {..} =
    let fee = selectWithdrawalFee userWalletCurrency
    in  calcFeeCents fee withdrawalRequestFrozenAmount

reqTransferAmount :: UserWallet -> WithdrawalRequest -> Int
reqTransferAmount w r =
    let fee    = reqFeeCents w r
        amount = withdrawalRequestFrozenAmount r - fee
    in  amount


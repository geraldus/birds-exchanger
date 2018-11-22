{-# LANGUAGE OverloadedStrings #-}
module Handler.Operator.WithdrawalRequest where


import           Import
import           Local.Persist.Deposit

import           Database.Persist.Sql           ( fromSqlKey
                                                , toSqlKey
                                                , rawSql
                                                )


getOperatorWithdrawalRequestsListR :: Handler Html
getOperatorWithdrawalRequestsListR = do
    requireStaffId
    list <-
        runDB $ rawSql s [] :: Handler
            [(Entity WithdrawalRequest, Entity UserWallet, Entity User)]
    defaultLayout $(widgetFile "operator/withdrawal-requests-list")
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
    wid  <- toSqlKey <$> runInputPost (ireq intField "withdrawal-id")
    time <- liftIO getCurrentTime
    runDB $ do
        insert $ WithdrawalAccept wid (pack $ show staffId) mStaffUserId
        update wid [WithdrawalRequestAccepted =. Just time]
    redirect OperatorWithdrawalRequestsListR

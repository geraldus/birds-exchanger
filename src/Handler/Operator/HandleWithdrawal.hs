{-# LANGUAGE OverloadedStrings #-}
module Handler.Operator.HandleWithdrawal where

import           Import
import           Local.Persist.Exchange ( ProfitType (..) )
import           Local.Persist.Wallet

import           Database.Persist.Sql   ( toSqlKey )


postOperatorAcceptWithdrawalRequestR :: Handler Html
postOperatorAcceptWithdrawalRequestR = do
    staffId <- requireStaffId
    let staff = pack $ show staffId
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
        insert $ WithdrawalAccept
            wid
            trans
            fee
            staff
            mStaffUserId
            time
        update
            wid
            [ WithdrawalRequestAccepted =. Just time
            , WithdrawalRequestStatus =. WsOperatorExecuted staff ]
        insert $ InnerProfitRecord
            (withdrawalRequestWalletTransactionReasonId request)
            (userWalletCurrency wallet)
            fee
            WithdrawalFee
    redirect OperatorWithdrawalRequestsListR


postOperatorDeclineWithdrawalRequestR :: Handler Html
postOperatorDeclineWithdrawalRequestR = do
    staffId   <- requireStaffId
    requestId <- fmap toSqlKey $ runInputPost $ ireq intField "request-id"
    reason    <- runInputPost $ ireq textareaField "reason"
    time      <- liftIO getCurrentTime
    runDB $ do
        request <- get404 requestId
        let walletId = withdrawalRequestWalletId request
        let amount = withdrawalRequestFrozenAmount request
        wallet <- get404 walletId
        let staff = pack . show $ staffId
        let before = userWalletAmountCents wallet
        update
            requestId
            [ WithdrawalRequestStatus =. WsOperatorRejected staff ]
        transactionReasonId <- insert $ WalletTransactionReason walletId
        insert $ WithdrawalReject
            requestId
            staff
            reason
            transactionReasonId
            time
        insert $ WalletBalanceTransaction
            walletId
            (BalanceWithdrawalReject amount)
            transactionReasonId
            before
            time
        update
            walletId
            [ UserWalletAmountCents +=. amount ]
    redirect OperatorDepositRequestsListR

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handler.Client.HandleWithdrawal where

import           Import

import           Local.Persist.Wallet   ( TransactionTypePlain (..),
                                          WalletTransactionType (..),
                                          WithdrawalStatus (..) )

import           Database.Persist.Sql   ( toSqlKey )


postClientCancelWithdrawalR :: Handler Html
postClientCancelWithdrawalR = do
    notFound
    requestId <- fmap toSqlKey $ runInputPost $ ireq intField "request-id"
    withClientRequest requestId $ \(Entity rid WithdrawalRequest{..}) -> do
        runDB $ do
            let wid = withdrawalRequestWalletId
            let a = withdrawalRequestFrozenAmount
            wallet <- get404 wid
            transactionReasonId <- insert $ WalletTransactionReason wid
            t <- liftIO getCurrentTime
            update rid [WithdrawalRequestStatus =. WsClientCancelled t]
            let b = userWalletAmountCents wallet
            let tid = transactionReasonId
            insert $ WithdrawalCancel rid tid t
            let typ = BalanceWithdrawalCancel a
                typPlain = WithdrawalCancellation
            insert $ WalletBalanceTransaction
                    wid typ tid b t typPlain
            update wid [UserWalletAmountCents +=. a]
        setMessageI MsgDepositCancelled
        redirect WithdrawalR

withClientRequest ::
       WithdrawalRequestId
    -> (Entity WithdrawalRequest -> Handler Html)
    -> Handler Html
withClientRequest rid action = do
    clientId <- requireClientId
    (r, w) <- runDB $ do
        request <- get404 rid
        wallet <- get404 . withdrawalRequestWalletId $ request
        return (request, wallet)
    if userWalletUserId w /= clientId
        then notFound
        else action (Entity rid r)

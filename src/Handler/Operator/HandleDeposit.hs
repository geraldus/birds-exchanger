{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handler.Operator.HandleDeposit where

import           Import
import           Local.Persist.ExchangeOrder ( ProfitType (..) )
import           Local.Persist.Wallet
import           Type.Money                  ( oneCoinCents )
import           Utils.Deposit

import           Database.Persist.Sql        ( toSqlKey )


postOperatorAcceptDepositRequestR :: Handler Html
postOperatorAcceptDepositRequestR = do
    staffId            <- requireStaffId
    (drId, realAmount) <- (\(idt, amt) -> (toSqlKey idt, amt)) <$> runInputPost
        ((,) <$> ireq intField "deposit-id" <*> ireq doubleField
                                                     "deposit-real-income"
        )
    mdepreq <- runDB $ get drId
    case mdepreq of
        Nothing                  -> notFound
        Just DepositRequest {..} -> do
            mue <- runDB . get $ depositRequestUserId
            case mue of
                Nothing -> notFound
                Just u  -> do
                    walletEntity <- getOrCreateWallet
                        depositRequestUserId
                        depositRequestTargetCurrency
                    let (Entity userWalletId userWallet) = walletEntity
                    runDB $ do
                        wtrId <- insert $ WalletTransactionReason userWalletId
                        update
                            drId
                            [ DepositRequestStatus
                                  =. OperatorAccepted (pack . show $ staffId)
                            ]
                        let realAmountCents =
                                floor (realAmount * fromIntegral oneCoinCents)
                            ratio = selectRatio
                                depositRequestCurrency
                                depositRequestTargetCurrency
                            fee = calcFeeCents
                                (selectDepositFee depositRequestCurrency)
                                realAmountCents
                            realAmountToConvert = realAmountCents - fee
                            targetAmountCents =
                                truncate
                                    $ fromIntegral realAmountToConvert
                                    * ratio
                        let realWalletIncome = targetAmountCents
                        let mStaffUserId = case staffId of
                                Left uid -> Just uid
                                _        -> Nothing
                        let time = userWalletCreated userWallet
                        _ <- insert $ AcceptedDeposit
                            drId
                            depositRequestCurrency
                            depositRequestTargetCurrency
                            realAmountCents
                            ratio
                            fee
                            userWalletId
                            (pack . show $ staffId)
                            mStaffUserId
                            time
                            wtrId
                        when (fee > 0) $ void . insert $
                            InnerProfitRecord
                                wtrId
                                depositRequestCurrency
                                fee
                                DepositFee
                        _ <- insert $ WalletBalanceTransaction
                            userWalletId
                            (BalanceDeposit realWalletIncome)
                            wtrId
                            (userWalletAmountCents userWallet)
                            time
                        update userWalletId
                               [UserWalletAmountCents +=. realWalletIncome]
                    redirect OperatorDepositRequestsListR


postOperatorDeclineDepositRequestR :: Handler Html
postOperatorDeclineDepositRequestR = do
    staffId   <- requireStaffId
    requestId <- fmap toSqlKey $ runInputPost $ ireq intField "request-id"
    reason    <- runInputPost $ ireq textareaField "reason"
    time      <- liftIO getCurrentTime
    runDB $ do
        get404 requestId
        update
            requestId
            [ DepositRequestStatus =. OperatorRejected (pack . show $ staffId) ]
        insert $ DepositReject
            requestId
            (pack . show $ staffId)
            reason
            time
    redirect OperatorDepositRequestsListR

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handler.Operator.AcceptDeposit where

import           Import
import           Local.Persist.Deposit
import           Local.Persist.Wallet
import           Type.Money            ( oneCoinCents )
import           Utils.Deposit
import           Utils.Money           ( truncCoins2Cents )

import           Database.Persist.Sql  ( toSqlKey )


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
                        -- TODO: FIXME: Save fee data
                        -- Save inner profit record
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
                                (selectFee depositRequestCurrency)
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
                        adid <- insert $ AcceptedDeposit
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
                        wtId <- insert $ WalletBalanceTransaction
                            userWalletId
                            (BalanceDeposit realWalletIncome)
                            wtrId
                            (userWalletAmountCents userWallet)
                            time
                        update userWalletId
                               [UserWalletAmountCents +=. realWalletIncome]
                    redirect OperatorDepositRequestsListR
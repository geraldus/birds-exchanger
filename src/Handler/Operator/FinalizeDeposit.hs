{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Operator.FinalizeDeposit where


import Import
import Local.Persist.Deposit
import Local.Persist.Wallet
import Utils.Deposit

import Database.Persist.Sql (toSqlKey)


postOperatorAcceptDepositRequestR :: Handler Html
postOperatorAcceptDepositRequestR = do
    staffId <- requireStaffId
    (drId, realAmount) <- (\(idt, amt) -> (toSqlKey idt, amt))
        <$> (runInputPost $ (,)
                <$> ireq intField "deposit-id"
                <*> ireq doubleField "deposit-real-income")
    mdepreq <- runDB $ get drId
    case mdepreq of
        Nothing -> notFound
        Just depreq@DepositRequest{..} -> do
            mue <- runDB . get $ depositRequestUserId
            case mue of
                Nothing -> notFound
                Just u -> do
                    walletTextId <- appNonce128urlT
                    time <- liftIO $ getCurrentTime
                    let newWallet = UserWallet
                            depositRequestUserId
                            depositRequestTargetCurrency
                            0
                            walletTextId
                            time
                    eitherWallet <- runDB $ insertBy newWallet
                    let (userWalletId, userWallet) = case eitherWallet of
                            Left (Entity wid w) -> (wid, w)
                            Right wid -> (wid, newWallet)
                    runDB $ do
                        -- TODO: FIXME: Save fee data
                        wtrId <- insert $ WalletTransactionReason userWalletId
                        update drId [ DepositRequestStatus =. OperatorAccepted (pack . show $ staffId)]
                        let realAmountCents = floor (realAmount * fromIntegral oneCent)
                            ratio = selectRatio' depositRequestCurrency depositRequestTargetCurrency
                            fee = calcFeeCents (selectFee depositRequestCurrency) realAmountCents
                            realAmountToConvert = realAmountCents - fee
                            targetAmountCents = truncate $ fromIntegral realAmountToConvert * ratio
                        let realWalletIncome = targetAmountCents
                        let mStaffUserId = case staffId of
                                Left uid -> Just uid
                                _        -> Nothing
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
                        update userWalletId [UserWalletAmountCents +=. realWalletIncome]
                    redirect OperatorDepositRequestsListR
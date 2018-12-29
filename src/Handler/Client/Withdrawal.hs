{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handler.Client.Withdrawal where


import           Form.Profile.Deposit
import           Import
import           Local.Persist.Wallet
import           Type.Money              ( Money (..) )
import           Type.Withdrawal



getWithdrawalR :: Handler Html
getWithdrawalR = do
    requireClientId
    (widget, enctype) <- generateFormPost withdrawalForm
    defaultLayout $ defaultWidget widget enctype Nothing

postWithdrawalCreateR :: Handler Html
postWithdrawalCreateR = do
    ((res, widget), enctype) <- runFormPost withdrawalForm
    userId <- requireClientId
    mayError <- return $ case res of
        FormSuccess _ -> Nothing
        FormMissing   -> Just  ["Не получены данные формы"]
        FormFailure e -> Just e
    case res of
        FormMissing -> defaultLayout $ defaultWidget widget enctype mayError
        FormFailure _ -> defaultLayout $ defaultWidget widget enctype mayError
        FormSuccess (WithdrawalM (Money amt c) adr) -> do
            w <- getOrCreateWallet userId c
            let wid = entityKey w
            let walletCents = userWalletAmountCents (entityVal w)
            if amt > walletCents
                then defaultLayout $ defaultWidget widget enctype (Just ["Недостаточно средств на счёте"])
                else do
                    time <- liftIO $ getCurrentTime
                    reasonId <- runDB . insert $ WalletTransactionReason wid
                    let record = WithdrawalRequest
                            wid
                            "TODO: FIXME:  Make a data type for withdrawal method"
                            adr
                            amt
                            time
                            reasonId
                            Nothing
                    let transaction = WalletBalanceTransaction
                            wid
                            (BalanceWithdrawal amt)
                            reasonId
                            walletCents
                            time
                    _ <- runDB $ do
                        insert record
                        insert transaction
                        update wid [UserWalletAmountCents -=. amt]
                    setMessage "Заявка на вывод успешно создана"
                    redirect HomeR

defaultWidget :: Widget -> Enctype -> Maybe [Text] -> Widget
defaultWidget widget enctype mayError = [whamlet|
    $maybe error <- mayError
        $forall e <- error
            <div .error .text-muted>#{e}
    <form method=post enctype=#{enctype} action=@{WithdrawalCreateR}>
        ^{widget}
        <button .btn.btn-primary .mt-2 type=submit>вывод
    |]

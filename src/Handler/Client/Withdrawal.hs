{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Client.Withdrawal where

import           Form.Profile.Withdrawal
import           Import
import           Local.Persist.Wallet
import           Type.Money              ( Money (..) )
import           Type.Withdrawal



getWithdrawalR :: Handler Html
getWithdrawalR = do
    requireClientId
    formId <- newIdent
    (widget, enctype) <- generateFormPost $ withdrawalForm formId
    defaultLayout $ defaultWidget formId widget enctype Nothing

postWithdrawalCreateR :: Handler Html
postWithdrawalCreateR = do
    requireClientId
    formId <- newIdent
    ((res, widget), enctype) <- runFormPost $ withdrawalForm formId
    userId <- requireClientId
    let mayError = case res of
            FormSuccess _ -> Nothing
            FormMissing   -> Just  ["Не получены данные формы"]
            FormFailure e -> Just e
    case res of
        FormMissing -> defaultLayout $ defaultWidget formId widget enctype mayError
        FormFailure _ -> defaultLayout $ defaultWidget formId widget enctype mayError
        FormSuccess (WithdrawalM (Money amt c) tm fee adr) -> do
            -- TODO: FIXME:  Check if Transfer Method is valid
            w <- getOrCreateWallet userId c
            let wid = entityKey w
            let walletCents = userWalletAmountCents (entityVal w)
            let amount2Freeze = amt + fee
            if amount2Freeze > walletCents
                then defaultLayout $
                        defaultWidget
                            formId
                            widget
                            enctype
                            (Just ["Недостаточно средств на счёте"])
                else do
                    time <- liftIO getCurrentTime
                    reasonId <- runDB . insert $ WalletTransactionReason wid
                    let record = WithdrawalRequest
                            wid
                            tm
                            adr
                            amt
                            amount2Freeze
                            fee
                            time
                            reasonId
                            Nothing
                    let transaction = WalletBalanceTransaction
                            wid
                            (BalanceWithdrawal amount2Freeze)
                            reasonId
                            walletCents
                            time
                    _ <- runDB $ do
                        insert record
                        insert transaction
                        update wid [UserWalletAmountCents -=. amount2Freeze]
                    setMessage "Заявка на вывод успешно создана"
                    redirect HomeR


defaultWidget :: Text -> Widget -> Enctype -> Maybe [Text] -> Widget
defaultWidget formId widget enctype mayError = [whamlet|
    $maybe error <- mayError
        <div .row>
            <div .col-10 .mx-auto>
                <div .alert.alert-warning>
                    $forall e <- error
                        <div .error>#{e}
    <form ##{formId} method=post enctype=#{enctype} action=@{WithdrawalCreateR} .col-12 .col-sm-10 .col-md-8 .mx-auto>
        ^{widget}
        <div .form-group .row .justify-content-center>
            <button .btn.btn-lg.btn-outline-primary .mt-2 type=submit>вывод
    |]

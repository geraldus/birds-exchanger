{-# LANGUAGE OverloadedStrings #-}
module Handler.Client.DepositConfirm where

import Import

import Local.Persist.Deposit
import Local.Persist.Currency


getDepositRequestConfirmationR :: Text -> Handler Html
getDepositRequestConfirmationR code = withRequest' code $ \(Entity _ t) -> do
    when (depositRequestStatus t /= New) $ do
        setMessage "Данная заявка проходит проверку..."
        redirect HomeR
    let transactionCode = depositRequestTransactionCode t
        paymentGuide = case depositRequestPaymentMethod t of
            FiatPM SberBankCard2CardFPM RUR -> [whamlet|
                <p>Переведите #
                    <span .text-monospace.font-weight-bold>#{cents2dblT (depositRequestCentsAmount t)}
                    \#{currSign (depositRequestCurrency t)}
                    \ на карту #
                    <span .font-weight-bold .text-monospace>XXXX XXXX XXXX XXXX
                <p>В комментарии платежа #
                    <span .font-weight-bold>ОБЯЗАТЕЛЬНО УКАЖИТЕ КОД ОПЕРАЦИИ!
                <div .alert .alert-warning>
                    <p .text-center>
                        <small>КОД ОПЕРАЦИИ
                    <div .text-monospace .text-center .h3>#{code}
                |]
            _ -> [whamlet|Приём средст временно приостановлен.  Попробуйте позже|]
    defaultLayout $(widgetFile "client/deposit-proceed")


postDepositConfirmRequestR :: Handler Html
postDepositConfirmRequestR = do
    code <- runInputPost $ ireq textField "transaction-code"
    withRequest' code $ \(Entity tid t) -> do
        runDB $ update tid [DepositRequestStatus =. ClientConfirmed]
        setMessage "Заявка проходит проверку"
        redirect HomeR


withRequest'
    :: Text
    -> (Entity DepositRequest -> Handler Html)
    -> Handler Html
withRequest' code action =
    requireClientId >> withRequest code action notFound

withRequest
    :: Text
    -> (Entity DepositRequest -> Handler Html)
    -> Handler Html
    -> Handler Html
withRequest code action fallback = do
    mdr <- runDB . getBy $ UniqueDepositRequest code
    case mdr of
        Nothing -> fallback
        Just e -> action e

{-# LANGUAGE OverloadedStrings #-}
module Handler.Client.DepositConfirm where

import Import


getDepositRequestConfirmationR :: Text -> Handler Html
getDepositRequestConfirmationR code = withRequest' code $ \(Entity _ t) -> do
    let transactionCode = depositRequestTransactionCode t
        paymentGuide = [whamlet|Тут инструкция ща будет )))|]
    defaultLayout $(widgetFile "client/deposit-proceed")


postDepositConfirmRequestR :: Handler Html
postDepositConfirmRequestR = do
    code <- runInputPost $ ireq textField "transaction-code"
    withRequest' code $ \(Entity tid t) -> runDB $ do
        update tid [DepositRequestVerified =. True]
        setMessage "Заявка проходит проверку"
        redirect HomeR


withRequest' code action = do
    requireClientId
    withRequest code action notFound

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

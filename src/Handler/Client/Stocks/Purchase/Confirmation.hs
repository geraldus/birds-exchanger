{-# LANGUAGE OverloadedStrings #-}
module Handler.Client.Stocks.Purchase.Confirmation where

import           Import

import           Handler.Client.Stocks.Purchase.Details ( queryClientPurchases )


postClientStocksPurchaseConfirmationR :: Text -> Handler TypedContent
postClientStocksPurchaseConfirmationR token = do
    client <- requireClientId
    res <- runInputPostResult $ ireq textField "payer-address"
    messageRenderer <- getMessageRender
    processFormResult res messageRenderer client
  where
    processFormResult FormMissing render _ = selectRep $ do
        let message = MsgAPIMissingFormData
        redirectWithMessages [ ("form", message) ] [ ] StocksR
        provideRep . pure $ object
            [ "status" .= ("fail" :: Text)
            , "message" .= render message ]
    processFormResult (FormFailure es) render _ = selectRep $ do
        let message = MsgAPIInvalidFormData
        redirectWithMessages
            [ ("form", message) ]
            (map ((,) "form-error" . toHtml) es)
            StocksR
        provideRep . pure $ object
            [ "status" .= ("fail" :: Text)
            , "message" .= render message
            , "errors" .= toJSON es ]
    processFormResult (FormSuccess payerWallet) render client = do
        res <- runDB $ queryClientPurchases client token
        case res of
            [] -> selectRep $ do
                let message = MsgAPIInvalidStocksPurchaseToken
                redirectWithMessages     [("form", message)] [ ] HomeR
                respondJSONErrorMessages (render message)    [ ]
            (Entity pid _, _) : _ -> do
                timeNow <- liftIO getCurrentTime
                runDB $ update
                    pid
                    [ StocksPurchaseUserConfirmed =. Just timeNow
                    , StocksPurchasePayerAddress =. Just payerWallet ]
                redirect (ClientStocksPurchaseDetailsR token)

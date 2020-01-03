{-# LANGUAGE OverloadedStrings #-}
module Handler.API.Render.Stocks.ListItem where

import           Import

import           Stocks.Widgets                ( purchaseStatusW )
import           Utils.App.Client              ( dateTimeRowW )
import           Utils.Database.User.Stocks    ( queryClientPurchasesByToken )
import           Utils.Stocks                  ( purchaseSignificantDate )

import           Text.Blaze.Html.Renderer.Text ( renderHtml )


getClientStocksPurchaseItemRenderR :: Handler TypedContent
getClientStocksPurchaseItemRenderR = do
    client <- requireClientId
    tkn <- runInputPostResult $ ireq textField "purchase-id"
    messageRender <- getMessageRender
    processFormResult tkn messageRender client
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
    processFormResult (FormSuccess token) render client = do
        res <- runDB $ queryClientPurchasesByToken client token
        case res of
            [] -> selectRep $ do
                let message = MsgAPIInvalidStocksPurchaseToken
                redirectWithMessages     [("form", message)] [ ] HomeR
                respondJSONErrorMessages (render message)    [ ]
            (Entity _ p, Entity _ s) : _ -> do
                widgetBody <- renderClientStocksPurchase p s
                selectRep . provideRep $ pure $ object
                    [ "success" .= ("ok" :: Text)
                    , "html" .= renderHtml widgetBody
                    ]

renderClientStocksPurchase :: StocksPurchase -> Stocks -> Handler Html
renderClientStocksPurchase p s = do
    renderUrl       <- getUrlRender
    renderUrlParams <- getUrlRenderParams
    htmlId          <- newIdent
    let purchaseId      = stocksPurchaseToken p
        stocksAmount    = stocksPurchaseAmount p
        stocksPackName  = stocksName s
        significantDate = dateTimeRowW (purchaseSignificantDate p)
        purchaseStatus  = purchaseStatusW htmlId p s
        url             = renderUrl $ ClientStocksPurchaseDetailsR purchaseId
    widgetBody <- widgetToPageContent $(widgetFile "client/stocks/list-item")
    return . pageBody widgetBody $ renderUrlParams
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Client.Stocks.Purchase.Details where

import           Import
import           Local.Persist.Currency     ( pzmC )
import           Stocks.Widgets             ( purchaseStatusW )
import           Utils.App.Client           ( dateTimeRowW )
import           Utils.Database.User.Stocks ( queryClientPurchasesByToken )
import           Utils.Stocks               ( purchaseSignificantDate )

import           Data.Aeson                 ( decode, encode )
import           Text.Julius                ( rawJS )


getClientStocksPurchaseDetailsR :: Text -> Handler TypedContent
getClientStocksPurchaseDetailsR token = do
    client <- requireClientId
    let isClientLoggedIn = True
    htmlId <- newIdent
    urlRender <- getUrlRender
    (_, stocks) <- getUserBalances
    withExistingClientPurchase client $ \(Entity _ p, Entity _ s) -> do
        let abr             = stocksAbbr s
        let purchaseDate    = (dateTimeRowW . purchaseSignificantDate) p
        let purchaseId      = stocksPurchaseToken p
        let packName        = stocksName s
        let amount          = show (stocksPurchaseAmount p)
        let purchaseStatus  = purchaseStatusW htmlId p s
        let clientConfirmed = isJust (stocksPurchaseUserConfirmed p)
        let guide           = paymentGuideW p s
        let pageUrl         = urlRender (ClientStocksPurchaseDetailsR token)
        let clientSocketUrl = urlRender ClientNotificationsWebSocketR
        let jsonStocksPurchases = decodeUtf8 . encode $
                map clientStocksToJSON stocks
        selectRep . provideRep . defaultLayout $ do
            setAppTitle [ MsgPageTitleStocksDetailsAbbr abr ]
            $(widgetFile "client/stocks/purchases/details")
  where
    withExistingClientPurchase u handler = do
        res <- runDB $ queryClientPurchasesByToken u token
        case res of
            value : _ -> handler value
            [] -> do
                addMessageI "stocks purchase" MsgMessageClientPurchaseNotFound
                notFound

    paymentGuideW p s = do
        render <- handlerToWidget getAmountRenderer
        guideId <- newIdent
        let addr :: [Text]
            addr = fromMaybe [] . decode $ encodeUtf8 . fromStrict $
                stocksPurchaseTransferAddress p
        let priceCents = stocksPrice s * stocksPurchaseAmount p
            price = render [] [] False pzmC priceCents
        let recipientAddress = recipientAddressW addr
        $(widgetFile "messages/fenix-stocks/purchase/guide")

    recipientAddressW [] = mempty
    recipientAddressW (address : extras) = [whamlet|
        <input .form-control readonly value="#{address}">
        $forall extra <- extras
            <small .form-text .text-muted .break-all>
                #{extra}
        |]

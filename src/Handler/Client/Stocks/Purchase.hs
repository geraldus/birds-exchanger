{-# LANGUAGE OverloadedStrings #-}
module Handler.Client.Stocks.Purchase where

import           Import                          hiding ( on, (==.) )

import           Local.Persist.Currency          ( CryptoCurrency (PZM) )
import           Local.Persist.TransferMethod    ( TransferMethod (CryptoTM) )
import           Type.App                        ( defaultSelectNextAddr,
                                                   paymentAddressAddressee )

import           Data.Aeson
import           Database.Esqueleto


postClientStocksPurchaseR :: Handler TypedContent
postClientStocksPurchaseR = do
    client <- requireClientId
    res <- runInputPostResult $ (,)
        <$> ireq textField "stocks-pack"
        <*> ireq intField "amount"
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
    processFormResult (FormSuccess (stocksPack, amount)) render client = do
        let abbreviation = case stocksPack of
                "fenix-start"    -> Just "FNXB"
                "fenix-standard" -> Just "FNXS"
                "fenix-premium"  -> Just "FNXP"
                _                -> Nothing
        case abbreviation of
            Nothing -> do
                let message = MsgAPIInvalidStocksPack stocksPack
                selectRep $ do
                    redirectWithMessages [ ("form", message) ] [ ] StocksR
                    respondJSONErrorMessages (render message) [ ]
            Just abr -> do
                stocksActives <- runDB $ getFenixActivesUnsafeDB abr
                let (stocks, actives) = stocksActives
                let stocksAvailable = (stocksActiveLeft . entityVal) actives
                if amount > stocksAvailable
                    then do
                        let message = MsgFormErrorExceededAvailableStocks
                        selectRep $ do
                            redirectWithMessages
                                [ ("form", message) ] [ ] StocksR
                            respondJSONErrorMessages (render message) [ ]
                    else do
                        addressee <-
                            (jsonAddressee <$>) <$> getNextPaymentAddressee
                                    defaultSelectNextAddr (CryptoTM PZM)
                        case addressee of
                            Nothing -> do
                                let message = MsgFormErrorNoPaymentAddressee
                                selectRep $ do
                                    redirectWithMessages
                                        [ ("form", message) ] [ ] StocksR
                                    respondJSONErrorMessages
                                        (render message) [ ]
                            Just adr -> do
                                purchaseDetails <-
                                    apiCreateUnconfirmedStocksPurchase
                                        stocks actives amount client adr
                                let token = stocksPurchaseToken $
                                        entityVal purchaseDetails
                                selectRep $ do
                                    provideRep $
                                        (redirect (ClientStocksPurchaseDetailsR token) :: Handler Html)
                                    provideRep . pure  $ object
                                        [ "status" .= ("OK" :: Text)
                                        , "data" .= toJSON purchaseDetails ]


apiCreateUnconfirmedStocksPurchase ::
       Entity Stocks
    -> Entity StocksActive
    -> Int
    -> UserId
    -> Text
    -> Handler (Entity StocksPurchase)
apiCreateUnconfirmedStocksPurchase (Entity s _) a n u w = do
    t <- liftIO getCurrentTime
    x <- appNonce128urlT
    let p = StocksPurchase
                u s n t w x Nothing Nothing Nothing Nothing Nothing Nothing
    pid <- runDB $ insert p
    return (Entity pid p)

getFenixActivesUnsafeDB ::
    MonadIO m => Text -> SqlPersistT m (Entity Stocks, Entity StocksActive)
getFenixActivesUnsafeDB abr = do
    res <- select . from $ \(s `LeftOuterJoin` a) -> do
        on (just (s ^. StocksId) ==. a ?. StocksActiveStock)
        where_ (s ^. StocksAbbr ==. val abr)
        return (s, a)
    case res of
        (s', Just a') : _ -> return (s', a')
        _                 -> error $ concat
            [ "Should never happen, stocks created during app start, "
            , "abbreviations also already filtered." ]

jsonAddressee :: PaymentAddress -> Text
jsonAddressee = decodeUtf8 . toStrict . encode . paymentAddressAddressee

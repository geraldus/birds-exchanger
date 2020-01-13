{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Handler.LP.LP0001 where

import           Import                         hiding ( on, (==.) )

import           Handler.Client.Stocks.Purchase ( apiCreateUnconfirmedStocksPurchase,
                                                  jsonAddressee,
                                                  runStocksAmountPostForm )
import           Local.Persist.Currency         ( CryptoCurrency (PZM) )
import           Local.Persist.TransferMethod   ( TransferMethod (CryptoTM) )
import           Local.Persist.UserRole         ( UserRole (Client) )
import           Type.App                       ( defaultSelectNextAddr )
import           Utils.Database.Password        ( getCredsByEmail )


postLPHandler0001R :: Handler TypedContent
postLPHandler0001R = do
    stockRes <- runStocksAmountPostForm
    usrRes <- runQuickRegisterPostForm
    let res = (,) <$> stockRes <*> usrRes
    messageRenderer <- getMessageRender
    processFormResult res messageRenderer
  where
    processFormResult (Left es) render = selectRep $ do
        let message = MsgAPIInvalidFormData
        redirectWithMessages
            [ ("form", message) ]
            (map (\(l, e) -> (l, toHtml e)) es)
            HomeR
        provideRep . pure $ object
            [ "status" .= ("fail" :: Text)
            , "message" .= render message
            , "errors" .= toJSON es ]
    processFormResult (Right ((s, a, q), (u, e, pwd))) render = do
        addressee <-
            (jsonAddressee <$>) <$> getNextPaymentAddressee
                    defaultSelectNextAddr (CryptoTM PZM)
        case addressee of
            Nothing -> do
                let message = MsgFormErrorNoPaymentAddressee
                selectRep $ do
                    redirectWithMessages
                        [ ("form", message) ] [ ] HomeR
                    respondJSONErrorMessages
                        (render message) [ ]
            Just adr -> do
                purchaseDetails <-
                    apiCreateUnconfirmedStocksPurchase
                        s a q (entityKey u) adr
                let token = stocksPurchaseToken $
                        entityVal purchaseDetails
                when (isJust pwd) $ do
                    let lgn = (emailEmail . entityVal) e
                        creds = Creds "prizm auth plugin" lgn [("email", lgn)]
                    setCreds False creds
                selectRep $ do
                    let url = ClientStocksPurchaseDetailsR token
                    provideRep $ (redirect url :: Handler Html)
                    provideRep . pure  $ object
                        [ "status" .= ("OK" :: Text)
                        , "data" .= toJSON purchaseDetails ]

runQuickRegisterPostForm :: Handler
    (Either [(Text, Text)] (Entity User, Entity Email, Maybe Text))
runQuickRegisterPostForm = do
    mr <- getMessageRender
    res <- runInputPostResult $ ireq textField "email"
    processFormResult res mr
  where
    processFormResult FormMissing render = do
        let message = MsgAPIMissingFormData
        return $ Left [ ("form", render message) ]
    processFormResult (FormFailure es) render = do
        let message = MsgAPIInvalidFormData
        return . Left $
            [ ("form", render message) ]
            <> (map ((,) "form-error") es)
    processFormResult (FormSuccess email) _ = do
        pwd <- appNonce128urlT
        vk  <- appNonce128urlT
        Right <$> (runDB $ queryGetCreateCreds email (pwd, vk))

queryGetCreateCreds ::
       MonadIO m
    => Text
    -> (Text, Text)
    -> SqlPersistT m (Entity User, Entity Email, Maybe Text)
queryGetCreateCreds login (pwd, vk) = do
    dbVals <- getCredsByEmail login
    case dbVals of
        []         -> createNewCreds login
        (e, u) : _ -> return $ (u, e, Nothing)
  where
    createNewCreds l = do
        let usr = User l (Just pwd) Client
        uid <- insert usr
        let eml = Email l (Just uid) (Just vk)
        eid <- insert eml
        return (Entity uid usr, Entity eid eml, Just pwd)

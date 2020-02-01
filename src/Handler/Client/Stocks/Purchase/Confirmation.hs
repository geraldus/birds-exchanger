{-# LANGUAGE OverloadedStrings #-}
module Handler.Client.Stocks.Purchase.Confirmation where

import           Import                                   hiding ( on, update,
                                                            (==.) )

import           Handler.Operator.Stocks.Purchase.Details ( queryPurchaseFullDetails )
import           Local.Persist.Notice                     ( NoticeSubject (NoticeSubjectOperatorStocksPurchaseConfirmed),
                                                            NoticeType (NoticeEmail) )
import           Local.Persist.UserRole                   ( UserRole (Operator) )
import           Utils.App.Common                         ( sendNoReplyEmail )
import           Utils.Common                             ( getFormatDateRender,
                                                            getFormatTimeRender )
import           Utils.Database.User.Stocks               ( queryClientPurchasesByToken )
import           Utils.Money                              ( cents2dblT )
import           Utils.QQ                                 ( stFile )
import           Utils.Stocks                             ( purchaseSignificantDate )

import           Data.Aeson                               ( encode )
import qualified Data.Text.Lazy                           as TL
import           Database.Esqueleto
import qualified Database.Persist                         as P
import           Text.Blaze.Html.Renderer.Text            ( renderHtml )
import           Text.Hamlet                              ( shamletFile )


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
        res <- runDB $ queryClientPurchasesByToken client token
        case res of
            [] -> selectRep $ do
                let message = MsgAPIInvalidStocksPurchaseToken
                redirectWithMessages     [("form", message)] [ ] HomeR
                respondJSONErrorMessages (render message)    [ ]
            (Entity pid _, _) : _ -> do
                timeNow <- liftIO getCurrentTime
                runDB $ P.update
                    pid
                    [ StocksPurchaseUserConfirmed P.=. Just timeNow
                    , StocksPurchasePayerAddress  P.=. Just payerWallet ]
                notifyOperatorStocksPurchase pid
                redirect (ClientStocksPurchaseDetailsR token)

notifyOperatorStocksPurchase :: StocksPurchaseId -> Handler ()
notifyOperatorStocksPurchase pid = do
    vals <- runDB . queryPurchaseFullDetails $ pid
    case vals of
        [] -> do
            addMessage "invalid-url" "No purchase found"
            redirect OperatorStocksPurchaseIndexR
        (p, Entity _ s, Entity _ a, Entity _ u, Entity _ e) : _ -> do
            typ           <- appType . appSettings <$> getYesod
            urlRender     <- getUrlRender
            messageRender <- getMessageRender
            renderDate    <- getFormatDateRender
            renderTime    <- getFormatTimeRender
            tn            <- liftIO getCurrentTime
            operators     <- ("heraldhoi@gmail.com" :) <$> operatorEmailsFromDB
            let packName = stocksName s
            let url      = urlRender (OperatorStocksPurchaseDetailsR (entityKey p))
                sigDate  = purchaseSignificantDate $ entityVal p
                date     = renderDate sigDate <> " " <> renderTime sigDate
            let txt      = textContent (s, a) p u e url date
                html     = htmlContent (s, a) p u e url date
            let contents = toStrict . decodeUtf8 . encode $ object
                    [ "subject"      .= subject
                    , "text-content" .= txt
                    , "html-content" .= html ]
                subject = messageRender $
                        MsgEmailSubjectOperatorPendingPurchase packName
            flip mapM_ operators $ \email -> do
                let n = Notice
                            NoticeEmail (Just NoticeSubjectOperatorStocksPurchaseConfirmed) email contents tn Nothing 0 Nothing (Just tn)
                _ <- runDB $ insert n
                void . liftIO $
                    sendNoReplyEmail typ messageRender email subject txt html
                -- ^ TODO update notice status

operatorEmailsFromDB :: Handler [Text]
operatorEmailsFromDB = do
    vals <- runDB queryActiveOperators
    return $ map (emailEmail . entityVal . snd) vals

queryActiveOperators :: MonadIO m => SqlPersistT m [(Entity User, Entity Email)]
queryActiveOperators = select . from  $
    \(u `InnerJoin` e) -> do
        on (just (u ^. UserId) ==. e ^. EmailUserId)
        where_ (u ^. UserRole ==. val Operator)
        return (u, e)

textContent ::
       (Stocks, StocksActive)
    -> Entity StocksPurchase
    -> User
    -> Email
    -> Text
    -> Text
    -> TL.Text
textContent (s, a) p u e url date = let
    (stocksPack, stocksAmount, cost, transferAddr, payer, username, stocksLeft)
        = renderValues (s, a) p u e
    in fromStrict $
        [stFile|templates/mail/operator/stocks/purchase-confirmed.text|]

htmlContent ::
       (Stocks, StocksActive)
    -> Entity StocksPurchase
    -> User
    -> Email
    -> Text
    -> Text
    -> TL.Text
htmlContent (s, a) p u e url date = let
    (stocksPack, stocksAmount, cost, transferAddr, payer, username, stocksLeft)
        = renderValues (s, a) p u e
    in renderHtml $(shamletFile "templates/mail/operator/stocks/purchase-confirmed.hamlet")


renderValues ::
       (Stocks, StocksActive)
    -> Entity StocksPurchase
    -> User
    -> Email
    -> (Text, String, Text, Text, Text, Text, String)
renderValues (s, a) (Entity _ p) _ e = let
    amount       = stocksPurchaseAmount p
    price        = stocksPrice s
    stocksPack   = stocksName s
    stocksAmount = show amount
    cost         = cents2dblT (amount * price)
    transferAddr = stocksPurchaseTransferAddress p
    payer        = fromMaybe "не указан" (stocksPurchasePayerAddress p)
    username     = emailEmail e
    stocksLeft   = show (stocksActiveLeft a)
    in (stocksPack, stocksAmount, cost, transferAddr, payer, username, stocksLeft)

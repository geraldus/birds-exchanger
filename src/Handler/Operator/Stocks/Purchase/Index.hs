{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module Handler.Operator.Stocks.Purchase.Index where

import           Import                 as I hiding ( on, (==.) )
import           Local.Persist.Currency ( pzmC )
import           Utils.App.Client       ( dateTimeRowW )
import           Utils.Money            ( cents2dblT, centsToCoins,
                                          fixedDoubleT )
import           Utils.Stocks           ( purchaseSignificantDate )
import           Utils.Type

import           Database.Esqueleto     as E
import           Database.Persist.Sql   ( fromSqlKey )
import           Text.Julius            ( rawJS )


getOperatorStocksPurchaseIndexR :: Handler Html
getOperatorStocksPurchaseIndexR = do
    requireOperatorId
    renderUrl <- getUrlRender
    list <- runDB queryPendingPurchasesDB
    let body = if length list > 0
            then mapM_ purchaseItemW list
            else [whamlet|
                <span .mt-2 .text-muted>
                    _{MsgNoOperationsYet}|]
    defaultLayout $ do
        $(widgetFile "operator/common")
        $(widgetFile "operator/stocks/purchase/index")
  where
    purchaseItemW ::
           ( Entity StocksPurchase
           , Entity Stocks
           , Entity StocksActive
           , Entity User
           , Entity Email )
        -> Widget
    purchaseItemW (Entity pid p, Entity _ s, Entity _ a, _, Entity _ e) = do
        renderAmount <- handlerToWidget getAmountRenderer
        htmlId <- newIdent
        let purchaseId      = fromSqlKey pid
            stocksAmount    = stocksPurchaseAmount p
            stocksPackName  = stocksName s
            significantDate = dateTimeRowW (purchaseSignificantDate p)
            priceCents      = stocksPurchaseAmount p * stocksPrice s
            price           = renderAmount [] [] False pzmC priceCents
            payerAddress    = fromMaybe "" (stocksPurchasePayerAddress p)
            payerEmail      = emailEmail e
            stocksLeft      = stocksActiveLeft a
            confirmText     = concat
                [ "Вы подтверждаете, что с кошелька\\n"
                , payerAddress <> " было перечислено\\n"
                , cents2dblT priceCents <> "PZM?\\n"
                , "Пользователю " <> emailEmail e <> " будет начислено:\\n"
                , fixedDoubleT 0 ((centsToCoins priceCents) / 10)
                , " ZLT токенов.\\n"
                , "Продолжить?"]
        $(widgetFile "operator/stocks/purchase/item")


queryPendingPurchasesDB :: MonadIO m => SqlPersistT m [PendingPurchaseDetails]
queryPendingPurchasesDB = select . from $
    \(e `InnerJoin` u `InnerJoin` p `InnerJoin` s `InnerJoin` a) -> do
        on (a ^. StocksActiveStock ==. s ^. StocksId)
        on (s ^. StocksId ==. p ^. StocksPurchaseStocks)
        on (p ^. StocksPurchaseUser ==. u ^. UserId)
        on (just (u ^. UserId) ==. e ^. EmailUserId)
        orderBy
            [ asc (p ^. StocksPurchaseUserConfirmed)
            , asc (p ^. StocksPurchaseCreated) ]
        where_
            (   (not_  $ E.isNothing (p ^. StocksPurchaseUserConfirmed))
            &&. (E.isNothing (p ^. StocksPurchaseCancelled))
            &&. (E.isNothing (p ^. StocksPurchaseAccepted))
            &&. (E.isNothing (p ^. StocksPurchaseAcceptedBy))
            &&. (E.isNothing (p ^. StocksPurchaseAcceptedByIdent))
            )
        return (p, s, a, u, e)

type PendingPurchaseDetails
    = (Ent StocksPurchase, Ent Stocks, Ent StocksActive, Ent User, Ent Email)

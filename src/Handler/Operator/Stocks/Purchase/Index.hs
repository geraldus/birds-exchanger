{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Handler.Operator.Stocks.Purchase.Index where

import           Import                 as I hiding ( on, (==.) )
import qualified Import                 as I

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
        payerAddress    = maybe "" (<> ", ") (stocksPurchasePayerAddress p)
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
        extra = itemExtra htmlId
    $(widgetFile "operator/stocks/purchase/item")
  where
    itemExtra :: Text -> Widget
    itemExtra htmlId = if isPending p
        then $(widgetFile "operator/stocks/purchase/confirm-button")
        else status p

    isPending :: StocksPurchase -> Bool
    isPending p =
           isJust      (stocksPurchaseUserConfirmed p)
        && I.isNothing (stocksPurchaseCancelled p)
        && I.isNothing (stocksPurchaseAccepted p)
        && I.isNothing (stocksPurchaseAcceptedBy p)
        && I.isNothing (stocksPurchaseAcceptedByIdent p)

    status :: StocksPurchase -> Widget
    status p
        | I.isNothing (stocksPurchaseUserConfirmed p) =
            [whamlet|_{MsgStocksNotConfirmedYet}|]
        | isJust (stocksPurchaseCancelled p) =
            [whamlet|_{MsgUserCancelled}|]
        | isJust (stocksPurchaseAccepted p) = do
            let ident = fromMaybe "<no name>" (stocksPurchaseAcceptedByIdent p)
            [whamlet|
                _{MsgProcessed}
                <br>
                #{ident}|]
        | otherwise = mempty


queryPendingPurchasesDB :: MonadIO m => SqlPersistT m [FullPurchaseDetails]
queryPendingPurchasesDB = select . from $
    \q@(e `InnerJoin` u `InnerJoin` p `InnerJoin` s `InnerJoin` a) -> do
        purchaseDataJoins q
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

purchaseDataJoins ::
       (   SqlExpr (Entity Email)
           `InnerJoin` SqlExpr (Entity User)
           `InnerJoin` SqlExpr (Entity StocksPurchase)
           `InnerJoin` SqlExpr (Entity Stocks)
           `InnerJoin` SqlExpr (Entity StocksActive)
       )
    -> SqlQuery ()
purchaseDataJoins =
    \(e `InnerJoin` u `InnerJoin` p `InnerJoin` s `InnerJoin` a) -> do
        on (a ^. StocksActiveStock ==. s ^. StocksId)
        on (s ^. StocksId ==. p ^. StocksPurchaseStocks)
        on (p ^. StocksPurchaseUser ==. u ^. UserId)
        on (just (u ^. UserId) ==. e ^. EmailUserId)


type FullPurchaseDetails
    = (Ent StocksPurchase, Ent Stocks, Ent StocksActive, Ent User, Ent Email)

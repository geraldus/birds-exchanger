{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
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
    let archiveLinkText = MsgPageTitleStocksArchive
        archiveRoute = OperatorStocksPurchaseArchiveR
        allLinkText = MsgPageTitleStocksAccepted
        allRoute = OperatorStocksPurchaseAcceptedR
        links = [whamlet|
            <a href=@{archiveRoute}>_{archiveLinkText}
            <a .ml-3 href=@{allRoute}>_{allLinkText}
            |]
    getOperatorStocksGenericRoute queryPendingPurchases links


getOperatorStocksPurchaseArchiveR :: Handler Html
getOperatorStocksPurchaseArchiveR = do
    let archiveLinkText = MsgPageTitleStocksPending
        archiveRoute = OperatorStocksPurchaseIndexR
        allLinkText = MsgPageTitleStocksAccepted
        allRoute = OperatorStocksPurchaseAcceptedR
        links = [whamlet|
            <a href=@{archiveRoute}>_{archiveLinkText}
            <a .ml-3 href=@{allRoute}>_{allLinkText}
            |]
    getOperatorStocksGenericRoute queryAllPurchases links

getOperatorStocksPurchaseAcceptedR :: Handler Html
getOperatorStocksPurchaseAcceptedR = do
    let newLinkText = MsgPageTitleStocksPending
        newRoute = OperatorStocksPurchaseIndexR
        allLinkText = MsgPageTitleStocksArchive
        allRoute = OperatorStocksPurchaseArchiveR
        links = [whamlet|
            <a href=@{newRoute}>_{newLinkText}
            <a .ml-3 href=@{allRoute}>_{allLinkText}
            |]
    getOperatorStocksGenericRoute queryAcceptedPurchases links


getOperatorStocksGenericRoute ::
       (MonadIO m, m ~ Handler)
    => SqlPersistT m [FullPurchaseDetails] -> Widget -> Handler Html
getOperatorStocksGenericRoute queryList links = do
    _ <- requireOperatorId
    renderUrl <- getUrlRender
    list <- runDB queryList
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
        creationDate    = stocksPurchaseCreated p
        stocksAmount    = stocksPurchaseAmount p
        stocksPackName  = stocksName s
        dateCreated     = dateTimeRowW creationDate
        priceCents      = stocksPurchaseAmount p * stocksPrice s
        price           = renderAmount [] [] False pzmC priceCents
        payerAddress    = maybe "" (<> ", ") (stocksPurchasePayerAddress p)
        payerEmail      = emailEmail e
        stocksLeft      = stocksActiveLeft a
        extraClasses    = purchaseStatusClasses p
        cancelable      = not (isAccepted p) && not (isCancelled p)
        confirmText     = concat
            [ "Вы подтверждаете, что с кошелька\\n"
            , payerAddress <> " было перечислено\\n"
            , cents2dblT priceCents <> "PZM?\\n"
            , "Пользователю " <> emailEmail e <> " будет начислено:\\n"
            , fixedDoubleT 0 ((centsToCoins priceCents) / 10)
            , " ZLT токенов.\\n"
            , "Продолжить?"]
        cancellationConfirmText = concat
            [ "Вы уверены, что хотите отменить зявку:\\n"
            , (pack . show $ stocksAmount) <> "шт. × " <> stocksPackName <> "\\n"
            , "от " <> payerAddress <> payerEmail <> "?" ]
        cancelButton = if cancelable
            then $(widgetFile "operator/stocks/purchase/cancel-button")
            else mempty
        extra = itemExtra htmlId
    $(widgetFile "operator/stocks/purchase/item")
  where
    itemExtra :: Text -> Widget
    itemExtra htmlId = if isPending p
        then $(widgetFile "operator/stocks/purchase/confirm-button")
        else statusW p

    purchaseStatusClasses :: StocksPurchase -> Text
    purchaseStatusClasses prc
        | isPending prc   = "pending"
        | isAccepted prc  = "accepted"
        | isCancelled prc = "cancelled"
        | otherwise       = "unconfirmed"

    isPending :: StocksPurchase -> Bool
    isPending prc =
           not (isAccepted prc)
        && not (isCancelled prc)
        && isJust (stocksPurchaseUserConfirmed prc)

    confirmedByUser :: StocksPurchase -> Bool
    confirmedByUser = isJust . stocksPurchaseUserConfirmed

    isAccepted :: StocksPurchase -> Bool
    isAccepted prc =
           notCancelled prc
        && isJust (stocksPurchaseAccepted prc)
        -- && isJust (stocksPurchaseAcceptedBy prc)
        && isJust (stocksPurchaseAcceptedByIdent prc)
        && isJust (stocksPurchaseUserConfirmed prc)

    notCancelled :: StocksPurchase -> Bool
    notCancelled x =
           I.isNothing (stocksPurchaseCancelled x)
        && I.isNothing (stocksPurchaseCancellationNote x)

    isCancelled :: StocksPurchase -> Bool
    isCancelled = I.isJust . stocksPurchaseCancelled


    statusW :: StocksPurchase -> Widget
    statusW prc
        | isJust (stocksPurchaseCancelled prc) = do
            [whamlet|
                _{MsgStocksPurchaseStatusCancelled}
                $maybe note <- stocksPurchaseCancellationNote prc
                    <br>
                    <small .text-muted>
                        #{note}
                |]
        | I.isNothing (stocksPurchaseUserConfirmed prc) =
            [whamlet|_{MsgStocksNotConfirmedYet}|]
        | isJust (stocksPurchaseAccepted prc) = do
            let ident = fromMaybe
                    "<no name>" (stocksPurchaseAcceptedByIdent prc)
                sigDate         = purchaseSignificantDate prc
                hasDateExtra    = sigDate == stocksPurchaseCreated prc
                significantDate = if hasDateExtra
                                  then mempty
                                  else dateTimeRowW sigDate
            [whamlet|
                <small>
                    <span>
                        _{MsgProcessed}
                        <br>
                         ^{significantDate}
                <span>
                    #{ident}|]
        | otherwise = mempty

queryAllPurchases :: MonadIO m => SqlPersistT m [FullPurchaseDetails]
queryAllPurchases = select . from $
    \q@(e `InnerJoin` u `InnerJoin` p `InnerJoin` s `InnerJoin` a) -> do
        purchaseDataJoins q
        orderBy
            [ desc (p ^. StocksPurchaseCreated) ]
        return (p, s, a, u, e)

queryPendingPurchases :: MonadIO m => SqlPersistT m [FullPurchaseDetails]
queryPendingPurchases = select . from $
    \q@(e `InnerJoin` u `InnerJoin` p `InnerJoin` s `InnerJoin` a) -> do
        purchaseDataJoins q
        orderBy
            [ asc (p ^. StocksPurchaseUserConfirmed)
            , asc (p ^. StocksPurchaseCreated) ]
        pendingPurchaseConditions q
        return (p, s, a, u, e)

queryAcceptedPurchases :: MonadIO m => SqlPersistT m [FullPurchaseDetails]
queryAcceptedPurchases = select . from $
    \q@(e `InnerJoin` u `InnerJoin` p `InnerJoin` s `InnerJoin` a) -> do
        purchaseDataJoins q
        orderBy
            [ asc (p ^. StocksPurchaseUserConfirmed)
            , asc (p ^. StocksPurchaseCreated) ]
        acceptedPurchaseConditions q
        return (p, s, a, u, e)


purchaseDataJoins ::
       (   SqlExpr (Entity Email)
           `InnerJoin` SqlExpr (Entity User)
           `InnerJoin` SqlExpr (Entity StocksPurchase)
           `InnerJoin` SqlExpr (Entity Stocks)
           `InnerJoin` SqlExpr (Entity StocksActive)
       )
    -> SqlQuery ()
purchaseDataJoins (e `InnerJoin` u `InnerJoin` p `InnerJoin` s `InnerJoin` a) =
    do
        on (a ^. StocksActiveStock ==. s ^. StocksId)
        on (s ^. StocksId ==. p ^. StocksPurchaseStocks)
        on (p ^. StocksPurchaseUser ==. u ^. UserId)
        on (just (u ^. UserId) ==. e ^. EmailUserId)

pendingPurchaseConditions ::
       (   SqlExpr (Entity Email)
           `InnerJoin` SqlExpr (Entity User)
           `InnerJoin` SqlExpr (Entity StocksPurchase)
           `InnerJoin` SqlExpr (Entity Stocks)
           `InnerJoin` SqlExpr (Entity StocksActive)
       )
    -> SqlQuery ()
pendingPurchaseConditions =
    \(_ `InnerJoin` _ `InnerJoin` p `InnerJoin` _ `InnerJoin` _) -> do
        where_
            (   (not_  $ E.isNothing (p ^. StocksPurchaseUserConfirmed))
            &&. (E.isNothing (p ^. StocksPurchaseCancelled))
            &&. (E.isNothing (p ^. StocksPurchaseAccepted))
            -- &&. (E.isNothing (p ^. StocksPurchaseAcceptedBy))
            &&. (E.isNothing (p ^. StocksPurchaseAcceptedByIdent))
            )

acceptedPurchaseConditions ::
       (   SqlExpr (Entity Email)
           `InnerJoin` SqlExpr (Entity User)
           `InnerJoin` SqlExpr (Entity StocksPurchase)
           `InnerJoin` SqlExpr (Entity Stocks)
           `InnerJoin` SqlExpr (Entity StocksActive)
       )
    -> SqlQuery ()
acceptedPurchaseConditions =
    \(_ `InnerJoin` _ `InnerJoin` p `InnerJoin` _ `InnerJoin` _) -> do
        where_
            (   (not_ $ E.isNothing (p ^. StocksPurchaseUserConfirmed))
            &&. (not_ $ E.isNothing (p ^. StocksPurchaseAccepted))
            &&. (E.isNothing (p ^. StocksPurchaseCancelled))
            )


type FullPurchaseDetails
    = (Ent StocksPurchase, Ent Stocks, Ent StocksActive, Ent User, Ent Email)


data PurchaseStatus
    = PurchaseCreated UTCTime
    | PurchaseConfirmed UTCTime
    | PurchaseCancelled UTCTime Text
    | PurchaseAccepted UTCTime (Maybe UserId) (Maybe Text)

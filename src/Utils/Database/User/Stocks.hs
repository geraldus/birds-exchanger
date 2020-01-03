module Utils.Database.User.Stocks where

import           Import.NoFoundation hiding ( isNothing, on, (==.) )

import           Database.Esqueleto


queryClientStocks ::
       MonadIO m
    => UserId -> SqlPersistT m [(Entity StocksPurchase, Entity Stocks)]
queryClientStocks uid = select . from $
    \(u `InnerJoin` p `InnerJoin` s) -> do
        on (s ^. StocksId ==. p ^. StocksPurchaseStocks)
        on (p ^. StocksPurchaseUser ==. u ^. UserId)
        where_
            (   (u ^. UserId ==. val uid)
            &&. not_ (isNothing (p ^. StocksPurchaseAccepted))
            &&. isNothing (p ^. StocksPurchaseCancelled)
            )
        orderBy [ desc (p ^. StocksPurchaseCreated) ]
        return (p, s)

queryClientPurchases ::
       MonadIO m
    => UserId -> SqlPersistT m [(Entity StocksPurchase, Entity Stocks)]
queryClientPurchases uid = select . from $
    \(u `InnerJoin` p `InnerJoin` s) -> do
        on (s ^. StocksId ==. p ^. StocksPurchaseStocks)
        on (p ^. StocksPurchaseUser ==. u ^. UserId)
        orderBy [ desc (p ^. StocksPurchaseCreated) ]
        where_ (u ^. UserId ==. val uid)
        return (p, s)

queryClientPurchasesByToken ::
       MonadIO m
    => UserId -> Text -> SqlPersistT m [(Entity StocksPurchase, Entity Stocks)]
queryClientPurchasesByToken uid t = select . from $
    \(u `InnerJoin` p `InnerJoin` s) -> do
        on (s ^. StocksId ==. p ^. StocksPurchaseStocks)
        on (p ^. StocksPurchaseUser ==. u ^. UserId)
        where_
            (   (p ^. StocksPurchaseToken ==. val t)
            &&. (u ^. UserId ==. val uid) )
        return (p, s)

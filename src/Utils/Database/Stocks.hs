module Utils.Database.Stocks where

import           Import.NoFoundation hiding ( isNothing, on, (==.) )

import           Database.Esqueleto


queryStocksActives ::
       MonadIO m
    => SqlPersistT m [(Entity StocksActive, Entity Stocks)]
queryStocksActives = select . from $
    \(a `InnerJoin` s) -> do
        on (a ^. StocksActiveStock ==. s ^. StocksId)
        return (a, s)
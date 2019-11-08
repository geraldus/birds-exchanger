module Utils.Database.Orders where

import           Import.NoFoundation    as I hiding ( (==.), (||.) )
import           Local.Persist.Exchange
import           Market.Type            ( OrderUserP )
import           Utils.Money

import           Database.Esqueleto     as E


selectActiveOrdersOf
    :: (MonadIO m)
    => ExchangePair
    -> SqlPersistT m [OrderUserP]
selectActiveOrdersOf p = select . from $ \(o, u) -> do
    where_ (
            (o ^. ExchangeOrderIsActive ==. val True)
        &&. (o ^. ExchangeOrderUserId   ==. u ^. UserId)
        &&. (
                (o ^. ExchangeOrderPair ==. val p)
            ||. (o ^. ExchangeOrderPair ==. val (flipPair p))))
    orderBy
        [ asc (o ^. ExchangeOrderNormalizedRatio)
        , asc (o ^. ExchangeOrderCreated) ]
    return (o, u)

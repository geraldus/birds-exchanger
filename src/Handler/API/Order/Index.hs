{-# LANGUAGE  DeriveGeneric #-}
module Handler.API.Order.Index where

import           Import                 as I hiding ( (==.), (||.) )
import           Local.Persist.Exchange
import           Utils.Money

import           Data.HashMap.Strict    as HMS
import           Database.Esqueleto     as E

type OrderUserP = (Entity ExchangeOrder, Entity User)

data DOMStats = DOMStats
    { domStatsPair     :: ExchangePair
    , domStatsDirect   :: HashMap Double (Int, Int, Int)
    , domStatsOpposite :: HashMap Double (Int, Int, Int)
    }
    deriving (Show, Generic)
instance ToJSON DOMStats

selectActiveOrdersOf
    :: ExchangePair
    -> SqlPersistT Handler [OrderUserP]
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


partByPair :: [OrderUserP] -> ([OrderUserP], [OrderUserP])
partByPair [] = ([], [])
partByPair (head'@(order, _):rest) =
    let pair = exchangeOrderPair $ entityVal order
    in flip partition
            (head':rest)
            $ \(v, _) -> exchangeOrderPair (entityVal v) == pair

reduceDOM :: [OrderUserP] -> Maybe DOMStats
reduceDOM [] = Nothing
reduceDOM (head'@(o, _):rest) =
    let pair = exchangeOrderPair $ entityVal o
    in Just $
        I.foldr
            (domReducer pair)
            (DOMStats pair HMS.empty HMS.empty)
            (head':rest)

domReducer :: ExchangePair -> OrderUserP -> DOMStats -> DOMStats
domReducer p ((Entity _ o), _) state
    | p == orderPair =
        updateDirectStats state currentStats
    | p == flipPair orderPair =
        updateOppositeStats state currentStats
    | otherwise = state
    where
        orderPair = exchangeOrderPair o
        amt = exchangeOrderAmountLeft o
        rat = exchangeOrderNormalizedRatio o
        k = normalizeRatio orderPair (exchangeOrderRatioNormalization o) rat
        mlt = multiplyCents k amt

        currentStats = (1, amt, mlt)
        updateDirectStats x y =
            updateDirect x (HMS.insertWith updateStats rat y (domStatsDirect x))
        updateOppositeStats x y =
            updateOpposite x (HMS.insertWith updateStats rat y (domStatsOpposite x))

        updateDirect x y = x { domStatsDirect = y }
        updateOpposite x y = x { domStatsOpposite = y }

        updateStats (newCnt, newSum, newMlt) (oldCnt, oldSum, oldMlt) =
            (oldCnt + newCnt, oldSum + newSum, newMlt + oldMlt)



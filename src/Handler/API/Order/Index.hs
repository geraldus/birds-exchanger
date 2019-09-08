{-# LANGUAGE DeriveGeneric #-}
module Handler.API.Order.Index where

import           Import                 as I hiding ( (==.), (||.) )
import           Local.Persist.Exchange
import           Utils.Money

import           Data.HashMap.Strict    as HMS
import           Database.Esqueleto     as E


type OrderUserP = (Entity ExchangeOrder, Entity User)

type Rate = Double
type OutAmountCents = Int
type InAmountCents = Int
type OrdersCount = Int

type Dom = (OrdersCount, OutAmountCents, InAmountCents)

type ExchangePairDom =
    HashMap Rate Dom

type DomStats = HashMap ExchangePair ExchangePairDom


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

emptyDomStats :: DomStats
emptyDomStats = HMS.empty

-- | Reduce orders list to Depth of Market stats
-- First argument is used to filter data source.  If it is non empty
-- it defines pairs filter.
-- When first argument is empty no filtering applies.
reduceDomStats
    :: [ExchangePair] -- list of desired exchange directions
    -> [OrderUserP]
    -> DomStats
reduceDomStats pairFilter = I.foldr reducePair emptyDomStats
    where
        reducePair :: OrderUserP -> DomStats -> DomStats
        reducePair (Entity _ o, _) ds =
            let (key, _, rate, _) = exParams o
            in if isKnownPair key
                then HMS.insertWith
                        (updatePair rate) key (pairSingleton o) ds
                else ds

        isKnownPair p
                | pairFilter == [] = True
                | otherwise = flip any pairFilter $
                        \f -> f == p || f == flipPair p

        pairSingleton :: ExchangeOrder -> ExchangePairDom
        pairSingleton o = HMS.singleton r (1, a, m)
            where
                (p, a, r, m) = exParams o
                n = normalizeRatio p (exchangeOrderRatioNormalization o) r


updatePair :: Rate -> ExchangePairDom -> ExchangePairDom -> ExchangePairDom
updatePair rate newState =
    HMS.insertWith updateRatioStats rate (newState ! rate)

updateRatioStats :: Dom -> Dom -> Dom
updateRatioStats (newCnt, newSum, newMlt) (oldCnt, oldSum, oldMlt) =
    (oldCnt + newCnt, oldSum + newSum, newMlt + oldMlt)

exParams :: ExchangeOrder -> (ExchangePair, OutAmountCents, Rate, InAmountCents)
exParams o =
    let p = exchangeOrderPair o
        a = exchangeOrderAmountLeft o
        r = exchangeOrderNormalizedRatio o
        k = normalizeRatio p (exchangeOrderRatioNormalization o) r
        m = multiplyCents k a
    in (p, a, r, m)
{-# LANGUAGE GADTs #-}
module Market.Functions where

import           Local.Persist.Exchange ( ExchangePair )
import           Market.Type
import           Model                  ( ExchangeOrder (..) )
import           Utils.Marshal          ( exParams )
import           Utils.Money            ( flipPair )

import           ClassyPrelude.Yesod    as P
import qualified Data.HashMap.Strict    as HMS


foldMarketOrders ::
    ( MonoFoldable mono
    , Element mono ~ (Entity ExchangeOrder, b) )
    => [ExchangePair]
    -> mono
    -> FullMarketDepth
foldMarketOrders dirs = foldr
    (updateMarketStats dirs . entityVal . fst)
    fullMarketEmptyStats

-- | Steppter function for folding full market orders to DOM stats
updateMarketStats ::
        [ExchangePair] -> ExchangeOrder -> FullMarketDepth ->  FullMarketDepth
updateMarketStats dirs o acc
    | isAskOrder =
        -- if exchange pair key is not present then we should insert
        -- single orders stats, there is all ok (well, it looks like)
        -- if we do have pair in our map... then we can safely omit
        -- new value in function and update oldStats in simplier way
        -- so we have _new old == Pair Stats and order ...
        HMS.insertWith
            exchangePairStatsUpdater
            expair
            (singleOrderExchangePairStats p o)
            acc
    | isBidOrder =
        HMS.insertWith
            exchangePairStatsUpdater
            expair
            (singleOrderExchangePairStats p o)
            acc
    | otherwise = acc
  where
        (p, a, r, m) = exParams o

        isAskOrder = p `elem` dirs

        -- "tricky" flip exchange pair if order is not ask order
        expair = if isAskOrder then p else flipPair p

        -- if it is not ask order `expair` will hold flipped pair already
        isBidOrder = not isAskOrder && expair `elem` dirs

        exchangePairStatsUpdater ::
                   DOMExchangePairStats
                -> DOMExchangePairStats
                -> DOMExchangePairStats
        exchangePairStatsUpdater _newStats oldStats
            | isAskOrder = updateAskSummary (a, r, m) oldStats
            | isBidOrder = updateBidSummary (a, r, m) oldStats
            | otherwise = oldStats

-- | Safe generator with single order as data source
-- singleOrderDOMExchangePairStats ::
--         ExchangePair -> ExchangeOrder -> DOMExchangePairStats
-- singleOrderDOMExchangePairStats exchangePair order = DOMExchangePairStats
--         { domExchangePairStatsKey = exchangePair
--         , domExchangePairStatsAsk = ask
--         , domExchangePairStatsBid = bid }
--   where
--         (ask, bid) = if exchangeOrderPair order == exchangePair
--             then (error "single ask", error "empty")
--             else (error "empty", error "single bid")

-- singleOrderDomExchangeDirectionStats :: ExchangeOrder -> DOMExchangeDirectionStats
-- singleOrderDomExchangeDirectionStats o = DOMExchangeDirectionStats
--         { domExchangeDirectionStatsMap =  o }

-- singleOrderMarketStats :: ExchangePair -> ExchangeOrder -> FullMarketDepth
-- singleOrderMarketStats dir order
--     | dir == expair = HMS.singleton expair (singleOrderDomStatsRateMap order)
--     where expair = exchangeOrderPair order

singleOrderDomStatsRateMap :: ExchangeOrder -> DOMStatsRateMap
singleOrderDomStatsRateMap o = HMS.singleton r (1, a, m)
    where (p, a, r, m) = exParams o

singleDomRateStats :: ExchangeOrder -> DOMRateStats
singleDomRateStats o =
    let (_, a, _, m) = exParams o
    in (1, a, m)

singleDomRateStats' :: (Int, Int) -> DOMRateStats
singleDomRateStats' (o, i) = (1, o, i)

-- | Reduce orders list to Depth of Market stats
-- First argument is used to filter data source.  If it is non empty
-- it defines pairs filter.
-- When first argument is empty no filtering applies.
reduceDomStats
    :: [ExchangePair] -- list of desired exchange directions
    -> [OrderUserP]
    -> DOMStats
reduceDomStats pairFilter = P.foldr reducePair emptyDomStats
    where
        reducePair :: OrderUserP -> DOMStats -> DOMStats
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

        pairSingleton :: ExchangeOrder -> DOMStatsRateMap
        pairSingleton o = HMS.singleton r (1, a, m)
            where
                (_p, a, r, m) = exParams o

updatePair ::
       Rate
    -> DOMStatsRateMap
    -> DOMStatsRateMap
    -> DOMStatsRateMap
updatePair rate newState =
    HMS.insertWith updateRatioStats rate (newState HMS.! rate)

updateRatioStats :: DOMRateStats -> DOMRateStats -> DOMRateStats
updateRatioStats (newCnt, newSum, newMlt) (oldCnt, oldSum, oldMlt) =
    (oldCnt + newCnt, oldSum + newSum, newMlt + oldMlt)

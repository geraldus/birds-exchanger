module Type.Market where

import           ClassyPrelude.Yesod    as P
import           Local.Persist.Exchange ( ExchangePair )
import           Model                  ( ExchangeOrder (..), User (..) )
import           Utils.Money            ( flipPair, multiplyCents,
                                          normalizeRatio )

import           Data.HashMap.Strict    as HMS


type OutAmountCents = Int

type InAmountCents = Int

type OrdersCount = Int

type Rate = Double

type Dom = (OrdersCount, OutAmountCents, InAmountCents)

type ExchangePairDom =
    HashMap Rate Dom

type DomStats = HashMap ExchangePair ExchangePairDom

type OrderUserP = (Entity ExchangeOrder, Entity User)


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
reduceDomStats pairFilter = P.foldr reducePair emptyDomStats
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
                (_p, a, r, m) = exParams o
                -- rate = normalizeRatio par (exchangeOrderRatioNormalization o) r


exParams :: ExchangeOrder -> (ExchangePair, OutAmountCents, Rate, InAmountCents)
exParams o =
    let p = exchangeOrderPair o
        a = exchangeOrderAmountLeft o
        r = exchangeOrderNormalizedRatio o
        k = normalizeRatio p (exchangeOrderRatioNormalization o) r
        m = multiplyCents k a
    in (p, a, r, m)


updatePair :: Rate -> ExchangePairDom -> ExchangePairDom -> ExchangePairDom
updatePair rate newState =
    HMS.insertWith updateRatioStats rate (newState ! rate)

updateRatioStats :: Dom -> Dom -> Dom
updateRatioStats (newCnt, newSum, newMlt) (oldCnt, oldSum, oldMlt) =
    (oldCnt + newCnt, oldSum + newSum, newMlt + oldMlt)

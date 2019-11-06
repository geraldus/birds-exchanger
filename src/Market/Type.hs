{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Market.Type
    ( FullMarketDepth
    , DOMExchangePairStats(..)
    , DOMExchangeAskStats
    , DOMExchangeBidStats
    , DOMExchangeDirectionStats(..)
    , DOMStatsRateMap
    , DOMRateStats
    , DOMStatsExchangeDirTotals(..)
    , IntAmountStats(..)
    -- * obsolete
    -- , ExchangePairMarketDepth
    , DOMStats
    -- * other / utils
    , OrderUserP
    , OutAmountCents
    , InAmountCents
    , Rate
    , OrdersCount
    -- * Functions
    , fullMarketEmptyStats
    , singleOrderExchangePairStats
    , mkDomExchangeAskStats
    , mkDomExchangeBidStats
    , domExchangeAskToDirectionStats
    , domExchangeBidToDirectionStats
    , updateAskSummary
    , updateBidSummary
    , emptyDomStats
    , emptyDomExchangePairStats
    , emptyExchangeDirectionStats
    , emptyDomStatsRateMap
    , emptyIntAmountStats
    , intStatsTuple
    , tupleIntStats
    , toJsonFullMarketDom
    ) where

import           ClassyPrelude.Yesod    as P
import           Local.Persist.Currency ( currencyCodeT )
import           Local.Persist.Exchange ( ExchangePair )
import           Model                  ( ExchangeOrder (..), User (..) )
import           Utils.Marshal          ( exParams )
import           Utils.Money            ( flipPair, unPairCurrency )

import qualified Data.HashMap.Strict    as HMS


-- | = Full Market

type FullMarketDepth = HashMap ExchangePair DOMExchangePairStats

-- | Type alias for mapping rate stats over exchange pairs
type DOMStats = HashMap ExchangePair DOMStatsRateMap

-- | = Exchange Pair

-- | DOM Exchange Pairs Stats
--   Exchange pair includes two directions: ask - pair as is
--   bid -- opposite (flipped) exchange pair
--
--   Statistics included:
--   * key - exchange pair
--   * ask stats - ask-orders DOM stats
--   * bid stats - bid-orders DOM stats
data DOMExchangePairStats = DOMExchangePairStats
        { domExchangePairStatsKey :: ExchangePair
        , domExchangePairStatsAsk :: DOMExchangeAskStats
        , domExchangePairStatsBid :: DOMExchangeBidStats
        }
    deriving (Show)

instance ToJSON DOMExchangePairStats where
    toJSON s =
        let key = domExchangePairStatsKey s
            (from, to) = unPairCurrency key
        in object
            [ "from" .= currencyCodeT from
            , "to" .= currencyCodeT to
            , "ask" .= toJSON (domExchangePairStatsAsk s)
            , "bid" .= toJSON (domExchangePairStatsBid s) ]


-- | = Exchange Directions (Ask & Bid)

-- Safe wrapper around 'DOMExchangeDirectionStats' holding
-- ask-orders DOM statistics
newtype DOMExchangeAskStats = DOMExchangeAskStats
        { domExchangeAskToDirectionStats :: DOMExchangeDirectionStats }
    deriving (Show)

instance ToJSON DOMExchangeAskStats where
    toJSON = toJSON . domExchangeAskToDirectionStats

-- Safe wrapper around 'DOMExchangeDirectionStats' holding
-- ask-orders DOM statistics
newtype DOMExchangeBidStats = DOMExchangeBidStats
        { domExchangeBidToDirectionStats :: DOMExchangeDirectionStats }
    deriving (Show)

instance ToJSON DOMExchangeBidStats where
    toJSON = toJSON . domExchangeBidToDirectionStats

-- | Market summary of arbitrary exchange direction.
--   Includes:
data DOMExchangeDirectionStats = DOMExchangeDirectionStats
        { domExchangeDirectionStatsMap         :: DOMStatsRateMap
        , domExchangeDirectionStatsOrdersCount :: OrdersCount
        , domExchangeDirectionStatsAmountOut   :: IntAmountStats
        , domExchangeDirectionStatsAmountIn    :: IntAmountStats }
    deriving (Show)

instance ToJSON DOMExchangeDirectionStats where
    toJSON s = object
            [ "map"   .= toJsonDomStatsRateMap (domExchangeDirectionStatsMap s)
            , "count" .= domExchangeDirectionStatsOrdersCount  s
            , "out"   .= toJSON (domExchangeDirectionStatsAmountOut s)
            , "in"    .= toJSON (domExchangeDirectionStatsAmountIn s)]

-- | = Depth of Market Rate Statistics
--   Represents summary statistics of arbitrary exchange pair rate.  It is
--   a triple of:
--   * count of orders for given exchange direction and rate
--   * total amount of outgoing values
--     (cents, first currency of exchange direction)
--   * total amount of expected income
--     (cents, second currency of exchange direction)
type DOMRateStats = (OrdersCount, OutAmountCents, InAmountCents)

-- | = Depth of Market Exchange Direction Statistics
--   Represents depth of market statistics of arbitrary
--   exchange direction as hash map where hash holds rate value and
--   values are depth of market rate stats 'DOMRateStats'
type DOMStatsRateMap = HashMap Rate DOMRateStats

data DOMStatsExchangeDirTotals = DOMStatsExchangeDirTotals
        { domStatsExchangeDirTotalsOrderCount :: OrdersCountTotal
        , domStatsExchangeDirTotalsOutAmount  :: IntAmountStats
        , domStatsExchangeDirTotalsInAmount   :: IntAmountStats }
    deriving (Show)

-- | Representation of outgoing or expected amount of value stats for
--   arbitrary rate and exchange direction
data IntAmountStats = IntAmountStats
        { intAmountStatsCnt :: Int
        , intAmountStatsSum :: Int
        , intAmountStatsMin :: Int
        , intAmountStatsMax :: Int
        }
    deriving (Show)

instance ToJSON IntAmountStats where
    toJSON (IntAmountStats c s n x) = object
            [ "count" .= toJSON c
            , "sum" .= toJSON s
            , "min" .= toJSON n
            , "max" .= toJSON x
            ]

type OrderUserP = (Entity ExchangeOrder, Entity User)

type OutAmountCents = Int

type InAmountCents = Int

type OrdersCount = Int

type Rate = Double

type OrdersCountTotal = Int

type MinAmountCents = Int

type MaxAmountCents = Int

type IntQuartet = (Int, Int, Int, Int)



-- | ** Utils

-- | *** Full Market Depths Statistics

fullMarketEmptyStats ::  FullMarketDepth
fullMarketEmptyStats = HMS.empty

emptyDomStats :: DOMStats
emptyDomStats = HMS.empty

-- | *** Exchange Pair Stats

singleOrderExchangePairStats :: ExchangePair -> ExchangeOrder -> DOMExchangePairStats
singleOrderExchangePairStats expair order = DOMExchangePairStats
    { domExchangePairStatsKey = expair
    , domExchangePairStatsAsk = mkDomExchangeAskStats expair [order]
    , domExchangePairStatsBid = mkDomExchangeBidStats expair [order]
    }


mkDomExchangeAskStats :: ExchangePair -> [ExchangeOrder] -> DOMExchangeAskStats
mkDomExchangeAskStats exchangePair list =
    DOMExchangeAskStats . blindlyMkDomExchangeDirectionStats $
        flip P.filter list $ \o ->
            exchangeOrderPair o == exchangePair

mkDomExchangeBidStats :: ExchangePair -> [ExchangeOrder] -> DOMExchangeBidStats
mkDomExchangeBidStats exchangePair list =
    DOMExchangeBidStats . blindlyMkDomExchangeDirectionStats $
        flip P.filter list $ \o ->
            exchangeOrderPair o == flipPair exchangePair

blindlyMkDomExchangeDirectionStats ::
        [ExchangeOrder] -> DOMExchangeDirectionStats
blindlyMkDomExchangeDirectionStats =
    P.foldr step emptyExchangeDirectionStats
  where
    step :: ExchangeOrder
         -> DOMExchangeDirectionStats
         -> DOMExchangeDirectionStats
    step o acc = acc
        { domExchangeDirectionStatsMap =
                updateRateMap r a m acc
        , domExchangeDirectionStatsOrdersCount =
                domExchangeDirectionStatsOrdersCount acc + 1
        , domExchangeDirectionStatsAmountOut =
                updateAmountOutStats acc a
        , domExchangeDirectionStatsAmountIn =
                updateAmountInStats acc m }
        where (p, a, r, m) = exParams o

    updateAmountOutStats acc = adjustIntAmountStats $
            domExchangeDirectionStatsAmountOut acc

    updateAmountInStats acc = adjustIntAmountStats $
            domExchangeDirectionStatsAmountIn acc

    updateRateMap rate a m acc = HMS.insertWith
            mappendRateSummaries
            rate
            (singleDomRateStats 1 a m)
            (domExchangeDirectionStatsMap acc)

emptyDomExchangePairStats :: ExchangePair -> DOMExchangePairStats
emptyDomExchangePairStats ep = DOMExchangePairStats
        { domExchangePairStatsKey = ep
        , domExchangePairStatsAsk =
            DOMExchangeAskStats emptyExchangeDirectionStats
        , domExchangePairStatsBid =
            DOMExchangeBidStats emptyExchangeDirectionStats
        }

exPairStatsTuple' ::
       DOMExchangePairStats
    -> ( ExchangePair
       , ( DOMStatsRateMap, OrdersCount, IntQuartet, IntQuartet )
       , ( DOMStatsRateMap, OrdersCount, IntQuartet, IntQuartet )
       )
exPairStatsTuple' s =
    let (p, d, o) = exPairStatsTuple s
    in (p, exDirStatsTuple' d, exDirStatsTuple' o)

exPairStatsTuple ::
       DOMExchangePairStats
    -> (ExchangePair, DOMExchangeDirectionStats, DOMExchangeDirectionStats)
exPairStatsTuple DOMExchangePairStats{..} =
    ( domExchangePairStatsKey
    , domExchangeAskToDirectionStats domExchangePairStatsAsk
    , domExchangeBidToDirectionStats domExchangePairStatsBid
    )

emptyExchangeDirectionStats :: DOMExchangeDirectionStats
emptyExchangeDirectionStats = DOMExchangeDirectionStats
        { domExchangeDirectionStatsMap         = emptyDomStatsRateMap
        , domExchangeDirectionStatsOrdersCount = 0
        , domExchangeDirectionStatsAmountOut   = emptyIntAmountStats
        , domExchangeDirectionStatsAmountIn    = emptyIntAmountStats
        }

updateAskSummary ::
       (OutAmountCents, Rate, InAmountCents)
    -> DOMExchangePairStats
    -> DOMExchangePairStats
updateAskSummary orderVals acc =
    let (_, stats, _) = exPairStatsTuple' acc
        newStats = updateDirectionSummary stats orderVals
    in acc { domExchangePairStatsAsk = DOMExchangeAskStats newStats }

updateBidSummary ::
       (OutAmountCents, Rate, InAmountCents)
    -> DOMExchangePairStats
    -> DOMExchangePairStats
updateBidSummary orderVals acc =
    let (_, _, stats) = exPairStatsTuple' acc
        newStats = updateDirectionSummary stats orderVals
    in acc { domExchangePairStatsBid = DOMExchangeBidStats newStats }

updateDirectionSummary ::
       ( HashMap Rate DOMRateStats , Int , IntQuartet , IntQuartet )
    -> (Int, Rate, Int)
    -> DOMExchangeDirectionStats
updateDirectionSummary stats vals =
    let (rateMap, cnt, outStatsTuple, inStatsTuple) = stats
        summaryTuple = (cnt, outStatsTuple, inStatsTuple)
        ((newCount, newOutStats, newInStats), newMap) =
                updateDirectionSummaryAndMap vals (summaryTuple, rateMap)
        newStats = DOMExchangeDirectionStats
                { domExchangeDirectionStatsMap = newMap
                , domExchangeDirectionStatsOrdersCount  = newCount
                , domExchangeDirectionStatsAmountOut = tupleIntStats newOutStats
                , domExchangeDirectionStatsAmountIn  = tupleIntStats newInStats
                }
    in newStats

updateDirectionSummaryAndMap ::
       (Int, Rate, Int)
    -> ( ( Int
         , (Int, Int, Int, Int)
         , (Int, Int, Int, Int)
         )
       , DOMStatsRateMap
       )
    -> ( ( Int
         , (Int, Int, Int, Int)
         , (Int, Int, Int, Int)
         )
       , DOMStatsRateMap
       )
updateDirectionSummaryAndMap (a, r, m) (summary, rateMap) =
    -- let newSummary = summary
    let newSummary = updateDirectionSummariesOutIn summary (a, m)
        newRateMap = updateRateMap r (singleDomRateStats 1 a m) rateMap
    in (newSummary, newRateMap)

updateDirectionSummariesOutIn ::
           (Int, IntQuartet, IntQuartet)
        -> (Int, Int)
        -> (Int, IntQuartet, IntQuartet)
updateDirectionSummariesOutIn (totalCount, outStats, inStats) (a, m) =
    let newTotalCount = totalCount + 1
        newOutStats = adjustIntAmountTuple outStats a
        newInStats = adjustIntAmountTuple inStats m
    in (newTotalCount, newOutStats, newInStats)


updateRateMap ::
       Rate
    -> DOMRateStats
    -> DOMStatsRateMap
    -> DOMStatsRateMap
updateRateMap = HMS.insertWith mappendRateSummaries


exDirStatsTuple ::
       DOMExchangeDirectionStats
    -> ( DOMStatsRateMap, OrdersCount, IntAmountStats, IntAmountStats )
exDirStatsTuple DOMExchangeDirectionStats{..} =
    ( domExchangeDirectionStatsMap
    , domExchangeDirectionStatsOrdersCount
    , domExchangeDirectionStatsAmountOut
    , domExchangeDirectionStatsAmountIn )

exDirStatsTuple' ::
       DOMExchangeDirectionStats
    -> ( DOMStatsRateMap, OrdersCount, IntQuartet, IntQuartet )
exDirStatsTuple' s =
    let ( m, c, o, i ) = exDirStatsTuple s
    in ( m, c, intStatsTuple o, intStatsTuple i)


-- | *** Rate Stats

-- singleOrderDomStatsRateMap :: ExchangeOrder -> Rate -> DOMStatsRateMap
-- singleOrderDomStatsRateMap o r = HMS.singleton r emptyDomRateStatsTriple

mappendRateSummaries :: DOMRateStats -> DOMRateStats -> DOMRateStats
mappendRateSummaries (nCnt, nOut, nIn) (oCnt, oOut, oIn) =
        (nCnt + oCnt, nOut + oOut, nIn + oIn)

singleDomRateStats :: Int -> Int -> Int -> DOMRateStats
singleDomRateStats cnt outa ina = (cnt, outa, ina)

emptyDomStatsRateMap :: DOMStatsRateMap
emptyDomStatsRateMap = HMS.empty

emptyDomRateStatsTriple :: DOMRateStats
emptyDomRateStatsTriple = (0, 0, 0)

emptyDomRateStats :: DOMRateStats
emptyDomRateStats = emptyDomRateStatsTriple

-- | *** General Stats

emptyIntAmountStats :: IntAmountStats
emptyIntAmountStats = IntAmountStats
        { intAmountStatsCnt = 0
        , intAmountStatsSum = 0
        , intAmountStatsMin = maxBound
        , intAmountStatsMax = 0
        }

intStatsTuple :: IntAmountStats -> (Int, Int, Int, Int)
intStatsTuple (IntAmountStats c s n x) = (c, s, n, x)

tupleIntStats :: (Int, Int, Int, Int) -> IntAmountStats
tupleIntStats (c, s, n, x) = IntAmountStats c s n x

adjustIntAmountStats :: IntAmountStats -> Int -> IntAmountStats
adjustIntAmountStats (IntAmountStats cnt sum' min' max') value =
    IntAmountStats
            (cnt + 1)
            (sum' + value)
            (min min' value)
            (max max' value)

adjustIntAmountTuple :: IntQuartet -> Int -> IntQuartet
adjustIntAmountTuple (cnt, sum', min', max') value =
    ( cnt + 1
    , sum' + value
    , min min' value
    , max max' value
    )

-- | === JSON

toJsonFullMarketDom :: FullMarketDepth -> Value
toJsonFullMarketDom = array . map fn . HMS.toList
  where
    fn :: (ExchangePair, DOMExchangePairStats) -> Value
    fn (expar, stats) =
        let (o, i) = unPairCurrency expar
        in object [ "from" .= currencyCodeT o
                  , "to" .= currencyCodeT i
                  , "stats" .= toJSON stats ]

toJsonDomRateStats :: DOMRateStats -> Value
toJsonDomRateStats (cnt, outa, ina) = object
    [ "count" .= cnt
    , "out" .= outa
    , "in" .= ina ]

toJsonDomStatsRateMap :: DOMStatsRateMap -> Value
toJsonDomStatsRateMap rm = object $ P.map
        (\(k, v) -> pack (show k) .= toJsonDomRateStats v)
        (HMS.toList rm)


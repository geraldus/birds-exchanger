module Utils.Marshal where

import           Local.Persist.Exchange ( ExchangePair )
import           Model                  ( ExchangeOrder (..) )
import           Utils.Money            ( multiplyCents, normalizeRatio )

import           ClassyPrelude


exParams :: ExchangeOrder -> (ExchangePair, Int, Double, Int)
exParams o =
    let p = exchangeOrderPair o
        a = exchangeOrderAmountLeft o
        r = exchangeOrderNormalizedRatio o
        k = normalizeRatio p (exchangeOrderRatioNormalization o) r
        m = multiplyCents k a
    in (p, a, r, m)

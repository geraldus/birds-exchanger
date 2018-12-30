module Utils.Money
    ( truncCoins2Cents
    )
where

import           Type.Money
import           Prelude


truncCoins2Cents :: Double -> Int
truncCoins2Cents x = truncate $ x * fromIntegral oneCoinCents
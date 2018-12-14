module Utils.Deposit where


import Prelude

import Type.Fee
import Local.Persist.Currency


selectFee :: Currency -> Fee
selectFee (FiatC RUR) = depositFeeRur
selectFee (CryptoC PZM) = depositFeePzm

selectRatio' :: Currency -> Currency -> Double
selectRatio' (CryptoC PZM) (FiatC RUR) = depositPzmRurRatio
selectRatio' (FiatC RUR) (CryptoC PZM) = depositRurPzmRatio
selectRatio' a b
    | a == b = 1
    | otherwise = error "Ratio selection not described yet!"

depositMinCentAmount :: Int
depositMinCentAmount = 500 * oneCoinCents -- 500 RUR

depositFeeRur :: Fee
depositFeeRur = Percent 0

depositFeePzm :: Fee
depositFeePzm = Percent 4


depositPzmRurRatio :: Double
depositPzmRurRatio = 25

depositRurPzmRatio :: Double
depositRurPzmRatio = 1 / depositPzmRurRatio


oneCoinCents :: Int
oneCoinCents = 100

-- TODO: FIXME: Rename to `trunc2cents`
doubleToCents :: Double -> Int
doubleToCents x = truncate $ x * fromIntegral oneCoinCents

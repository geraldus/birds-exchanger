module Utils.Deposit where

import           Prelude

import           Type.Money
import           Type.Fee
import           Local.Persist.Currency


-- TODO: FIXME: Make deposit and withdrawal fees configurable via admin's UI


selectDepositFee :: Currency -> Fee
selectDepositFee (FiatC   RUR) = depositFeeRur
selectDepositFee (CryptoC PZM) = depositFeePzm
selectDepositFee c = error $ "No deposit fee rules for " <> show c

selectRatio :: Currency -> Currency -> Double
selectRatio (CryptoC PZM) (FiatC   RUR) = depositPzmRurRatio
selectRatio (FiatC   RUR) (CryptoC PZM) = depositRurPzmRatio
selectRatio a b | a == b    = 1
                 | otherwise = error "Ratio selection not described yet!"

depositRurMinCentsAmount :: Int
depositRurMinCentsAmount = 500 * oneCoinCents -- 500 RUR

depositPzmMinCentsAmount :: Int
depositPzmMinCentsAmount = 50 * oneCoinCents

depositFeeRur :: Fee
depositFeeRur = defRurDepositFee

depositFeePzm :: Fee
depositFeePzm = defPzmDepositFee

defRurDepositFee :: Fee
defRurDepositFee = Percent 0

defPzmDepositFee :: Fee
defPzmDepositFee = Percent 3

depositPzmRurRatio :: Double
depositPzmRurRatio = 25

depositRurPzmRatio :: Double
depositRurPzmRatio = 1 / depositPzmRurRatio

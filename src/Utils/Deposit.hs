module Utils.Deposit where

import           Prelude

import           Local.Params           ( defPzmDepositFee, defRurDepositFee )
import           Local.Persist.Currency
import           Type.Fee


-- TODO: FIXME: Make deposit and withdrawal fees configurable via admin's UI


selectDepositFee :: Currency -> Fee
selectDepositFee (FiatC   RUR) = defRurDepositFee
selectDepositFee (CryptoC PZM) = defPzmDepositFee
selectDepositFee c             = error $ "No deposit fee rules for " <> show c

selectRatio :: Currency -> Currency -> Double
selectRatio (CryptoC PZM) (FiatC   RUR) = depositPzmRurRatio
selectRatio (FiatC   RUR) (CryptoC PZM) = depositRurPzmRatio
selectRatio a b | a == b    = 1
                 | otherwise = error "Ratio selection not described yet!"

depositPzmRurRatio :: Double
depositPzmRurRatio = 25

depositRurPzmRatio :: Double
depositRurPzmRatio = 1 / depositPzmRurRatio

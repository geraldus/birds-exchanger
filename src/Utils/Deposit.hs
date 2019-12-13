module Utils.Deposit where

import           Prelude

import           Local.Params           ( defOurDepositFee, defPzmDepositFee,
                                          defRurDepositFee )
import           Local.Persist.Currency
import           Type.Fee


-- TODO: FIXME: Make deposit and withdrawal fees configurable via admin's UI

selectDepositFee :: Currency -> Fee
selectDepositFee (FiatC   RUR) = defRurDepositFee
selectDepositFee (CryptoC PZM) = defPzmDepositFee
selectDepositFee (CryptoC OUR) = defOurDepositFee
selectDepositFee c             = error $ "No deposit fee rules for " <> show c


depositPzmRurRatio :: Double
depositPzmRurRatio = 25

depositRurPzmRatio :: Double
depositRurPzmRatio = 1 / depositPzmRurRatio

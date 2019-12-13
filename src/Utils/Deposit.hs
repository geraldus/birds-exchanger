module Utils.Deposit where

import           Prelude

import           Local.Params           ( defOuroDepositFee, defPzmDepositFee,
                                          defRubDepositFee )
import           Local.Persist.Currency
import           Type.Fee


-- TODO: FIXME: Make deposit and withdrawal fees configurable via admin's UI

selectDepositFee :: Currency -> Fee
selectDepositFee (FiatC   RUB) = defRubDepositFee
selectDepositFee (CryptoC PZM) = defPzmDepositFee
selectDepositFee (CryptoC OURO) = defOuroDepositFee
selectDepositFee c             = error $ "No deposit fee rules for " <> show c

depositPzmRubRatio :: Double
depositPzmRubRatio = 25

depositRubPzmRatio :: Double
depositRubPzmRatio = 1 / depositPzmRubRatio

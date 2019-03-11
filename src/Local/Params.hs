module Local.Params where

import           Prelude    ( Int, (*) )
import           Type.Fee   ( Fee (..) )
import           Type.Money ( oneCoinCents )


-- TODO: FIXME: Make an MVar or something and expand as app property.
-- Allow possibility to change this value on the fly
defaultExchangeFee :: Fee
defaultExchangeFee = Percent 1

defRurWithdrawalFee :: Fee
defRurWithdrawalFee = Percent 2.0

defPzmWithdrawalFee :: Fee
defPzmWithdrawalFee = Percent 0.0

defRurDepositFee :: Fee
defRurDepositFee = Percent 0

defPzmDepositFee :: Fee
defPzmDepositFee = Percent 2

depositRurMinCentsAmount :: Int
depositRurMinCentsAmount = 500 * oneCoinCents -- 500 RUR

depositPzmMinCentsAmount :: Int
depositPzmMinCentsAmount = 50 * oneCoinCents

-- TODO: FIXME: Configure minimum withdrawal bounds

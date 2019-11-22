module Local.Params where

import           Local.Persist.Currency ( Currency (..), ouroC, pzmC, rubC )
import           Prelude                ( Int, otherwise, (*), (==) )
import           Type.Fee               ( Fee (..) )
import           Type.Money             ( oneCoinCents )


-- TODO: FIXME: Make an MVar or something and expand as app property.
-- Allow possibility to change this value on the fly
defaultExchangeFee :: Fee
defaultExchangeFee = Percent 1.0

defRurWithdrawalFee :: Fee
defRurWithdrawalFee = Percent 2.0

defPzmWithdrawalFee :: Fee
defPzmWithdrawalFee = Percent 0.0

defOurWithdrawalFee :: Fee
defOurWithdrawalFee = Percent 0.0

defRurDepositFee :: Fee
defRurDepositFee = Percent 0.0

defPzmDepositFee :: Fee
defPzmDepositFee = Percent 0.5

defOurDepositFee :: Fee
defOurDepositFee = Percent 0.5


depositRurMinCentsAmount :: Int
depositRurMinCentsAmount = 500 * oneCoinCents -- 500 RUR

depositPzmMinCentsAmount :: Int
depositPzmMinCentsAmount = 10 * oneCoinCents

depositOurMinCentsAmount :: Int
depositOurMinCentsAmount = 10 * oneCoinCents


currencyDefaultMinimalDeposit :: Currency -> Int
currencyDefaultMinimalDeposit c
    | c == pzmC  = depositPzmMinCentsAmount
    | c == ouroC = depositOurMinCentsAmount
    | c == rubC  = depositRurMinCentsAmount
    | otherwise  = 199999999990

defaultWalletCurrencies :: [Currency]
defaultWalletCurrencies = [ rubC, pzmC, ouroC ]

-- TODO: FIXME: Configure minimum withdrawal bounds

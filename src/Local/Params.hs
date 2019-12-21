module Local.Params where

import           Local.Persist.Currency ( Currency (..), ouroC, pzmC, rubC )
import           Prelude                ( Int, Maybe (..), otherwise, (*),
                                          (==) )
import           Type.Fee               ( Fee (..) )
import           Type.Money             ( Percent, mkPercent, oneCoinCents )


-- TODO: FIXME: Make an MVar or something and expand as app property.
-- Allow possibility to change this value on the fly
defaultExchangeFee :: Fee
defaultExchangeFee = Percent 1.0

defRubWithdrawalFee :: Fee
defRubWithdrawalFee = Percent 2.0

defPzmWithdrawalFee :: Fee
defPzmWithdrawalFee = Percent 0.0

defOuroWithdrawalFee :: Fee
defOuroWithdrawalFee = Percent 0.0

defRubDepositFee :: Fee
defRubDepositFee = Percent 0.0

defPzmDepositFee :: Fee
defPzmDepositFee = Percent 0.5

defOuroDepositFee :: Fee
defOuroDepositFee = Percent 0.5


depositRubMinCentsAmount :: Int
depositRubMinCentsAmount = 300 * oneCoinCents -- 300 RUB

depositPzmMinCentsAmount :: Int
depositPzmMinCentsAmount = 10 * oneCoinCents

depositOuroMinCentsAmount :: Int
depositOuroMinCentsAmount = 10 * oneCoinCents


currencyDefaultMinimalDeposit :: Currency -> Int
currencyDefaultMinimalDeposit c
    | c == pzmC  = depositPzmMinCentsAmount
    | c == ouroC = depositOuroMinCentsAmount
    | c == rubC  = depositRubMinCentsAmount
    | otherwise  = 199999999990

defaultWalletCurrencies :: [Currency]
defaultWalletCurrencies = [ rubC, pzmC ]
-- TODO: DEFAULT WALLET CURRENCIES SHOULD ALTER FROM PROJECT TO PROJECT

defaultParaMiningDelaySeconds :: Int
defaultParaMiningDelaySeconds = 10

defaultCurrencyMonthlyParamining :: Currency -> Maybe Percent
defaultCurrencyMonthlyParamining c
    | c == pzmC = Just (mkPercent 12)
    | otherwise = Nothing

-- TODO: FIXME: Configure minimum withdrawal bounds

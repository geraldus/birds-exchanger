module Utils.Withdrawal
    ( defRurWithdrawalFee
    , defPzmWithdrawalFee
    , selectWithdrawalFee
    )
where

import           ClassyPrelude.Yesod

import           Local.Params
import           Local.Persist.Currency ( CryptoCurrency (..), Currency (..),
                                          FiatCurrency (..) )
import           Type.Fee               ( Fee (..) )


selectWithdrawalFee :: Currency -> Fee
selectWithdrawalFee (FiatC   RUR) = defRurWithdrawalFee
selectWithdrawalFee (CryptoC PZM) = defPzmWithdrawalFee
selectWithdrawalFee (CryptoC OUR) = defOurWithdrawalFee
selectWithdrawalFee c             = error $ "no default fee for " <> show c

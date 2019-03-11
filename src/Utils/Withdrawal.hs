module Utils.Withdrawal
    ( defRurWithdrawalFee
    , defPzmWithdrawalFee
    , selectWithdrawalFee
    )
where

import           ClassyPrelude.Yesod

import           Local.Params           ( defPzmWithdrawalFee,
                                          defRurWithdrawalFee )
import           Local.Persist.Currency ( CryptoCurrency (..), Currency (..),
                                          FiatCurrency (..) )
import           Type.Fee               ( Fee (..) )


selectWithdrawalFee :: Currency -> Fee
selectWithdrawalFee (FiatC   RUR) = defRurWithdrawalFee
selectWithdrawalFee (CryptoC PZM) = defPzmWithdrawalFee
selectWithdrawalFee c             = error $ "no default fee for " <> show c

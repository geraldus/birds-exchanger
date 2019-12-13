module Utils.Withdrawal
    ( defRubWithdrawalFee
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
selectWithdrawalFee (FiatC   RUB) = defRubWithdrawalFee
selectWithdrawalFee (CryptoC PZM) = defPzmWithdrawalFee
selectWithdrawalFee (CryptoC OURO) = defOuroWithdrawalFee
selectWithdrawalFee c             = error $ "no default fee for " <> show c

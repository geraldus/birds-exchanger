module Utils.Withdrawal
    ( defRurWithdrawalFee
    , defPzmWithdrawalFee
    , selectWithdrawalFee
    )
where

import ClassyPrelude.Yesod

import           Type.Fee                       ( Fee(..) )
import           Local.Persist.Currency         ( Currency(..)
                                                , FiatCurrency(..)
                                                , CryptoCurrency(..)
                                                )


selectWithdrawalFee :: Currency -> Fee
selectWithdrawalFee (FiatC   RUR) = defRurWithdrawalFee
selectWithdrawalFee (CryptoC PZM) = defPzmWithdrawalFee
selectWithdrawalFee c             = error $ "no default fee for " <> show c


defRurWithdrawalFee :: Fee
defRurWithdrawalFee = Percent 2.0

defPzmWithdrawalFee :: Fee
defPzmWithdrawalFee = Percent 0.0

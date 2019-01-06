module Utils.Withdrawal
    ( defRurWithdrawalFee
    , defPzmWithdrawalFee
    )
where

import           Type.Fee                       ( Fee(..) )


defRurWithdrawalFee :: Fee
defRurWithdrawalFee = Percent 2.0

defPzmWithdrawalFee :: Fee
defPzmWithdrawalFee = Percent 0.0

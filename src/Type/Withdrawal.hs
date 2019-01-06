module Type.Withdrawal
    ( WithdrawalM(..)
    )
where

import           Local.Persist.Currency
import           Type.Money

import           ClassyPrelude.Yesod


-- | This data type holds pieces of data minimally required to
--   create withdrawal request
data WithdrawalM = WithdrawalM
    Money
    -- ^ desired amount to withdraw
    TransferMethod
    -- ^ preferred way to transfer money
    Int
    -- ^ fee amount shown in form
    Text
    -- ^ transfer address
    deriving Show

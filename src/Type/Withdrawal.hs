module Type.Withdrawal
    ( WithdrawalM(..)
    )
where

import           ClassyPrelude.Yesod

import           Local.Persist.TransferMethod
import           Type.Money


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

module Type.Withdrawal
    ( WithdrawalM(..)
    )
where

import           Local.Persist.Currency
import           Type.Money

import           ClassyPrelude.Yesod


data WithdrawalM = WithdrawalM Money TransferMethod Text deriving Show

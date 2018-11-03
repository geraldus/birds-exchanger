module Local.Persist.Wallet where


import Prelude

import Database.Persist.TH


data WalletTransactionType
    -- | Int's are money amount in cents.  All income operations
    --   must have positive sign; outcome -- negative.
    = BalanceDeposit Int
    | BalanceWithdrawal Int
    | BalanceWithdrawalCancel Int
    | ExchangeFreeze Int
    | ExchangeReturn Int
    | ExchangeTransfer Int
    | ExchangeIncome Int
    | Penality Int
    | Bonus Int
    deriving (Show, Read, Eq)
derivePersistField "WalletTransactionType"
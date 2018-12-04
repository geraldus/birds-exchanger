module Local.Persist.Wallet where


import Prelude

import Database.Persist.TH


data WalletTransactionType
    -- | Int's are money amount in cents.  All income operations
    --   must have positive sign; outcome -- negative.
    = BalanceDeposit Int          -- ^ + positive
    | BalanceWithdrawal Int       -- ^ - negative
    | BalanceWithdrawalCancel Int -- ^ + positive
    | ExchangeFreeze Int          -- ^ - negative
    | ExchangeReturn Int          -- ^ + positive
    | ExchangeExchange Int        -- ^ + positive
    | Penality Int                -- ^ - negative
    | Bonus Int                   -- ^ + positive
    deriving (Show, Read, Eq)
derivePersistField "WalletTransactionType"
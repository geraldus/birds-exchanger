module Utils.Type (
    -- * Useful type name shortands.
    Ent,
    Wal,
    ExOrd,
    WReq,
    BTrans
) where

import Import.NoFoundation


type Ent = Entity

type Wal = UserWallet

type ExOrd = ExchangeOrder

type WReq = WithdrawalRequest

type BTrans = WalletBalanceTransaction

module Local.Persist.Wallet where

import           ClassyPrelude.Yesod


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


data DepositRequestStatus
    = New
    | ClientConfirmed
    | ClientCancelled UTCTime
    | OperatorRejected Text
    | OperatorAccepted Text
    | OperatorArchived Text
    | TimeoutArchieved Text
    deriving (Show, Read, Eq)
derivePersistField "DepositRequestStatus"

data WithdrawalStatus
    = WsNew
    | WsClientCancelled Text
    | WsOperatorRejected Text
    | WsOperatorExecuted Text
    | WsOperatorArchived Text
    | WsTimeoutArchieved Text
    deriving (Show, Read, Eq)
derivePersistField "WithdrawalStatus"

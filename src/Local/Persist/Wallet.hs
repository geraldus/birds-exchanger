{-# LANGUAGE DeriveGeneric #-}
module Local.Persist.Wallet where

import           ClassyPrelude.Yesod
import           Data.Aeson


data WalletTransactionType
    -- | Int's are money amount in cents.  All income operations
    --   must have positive sign; outcome -- negative.
    = BalanceDeposit Int          -- ^ + positive
    | BalanceWithdrawal Int       -- ^ - negative
    | BalanceWithdrawalCancel Int -- ^ + positive
    | BalanceWithdrawalReject Int -- ^ + positive
    | ExchangeFreeze Int          -- ^ - negative
    | ExchangeReturn Int          -- ^ + positive
    | ExchangeExchange Int        -- ^ + positive
    | Penality Int                -- ^ - negative
    | Bonus Int                   -- ^ + positive
    deriving (Generic, Show, Read, Eq)
derivePersistField "WalletTransactionType"

instance ToJSON WalletTransactionType where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON WalletTransactionType


data DepositRequestStatus
    = New
    | ClientConfirmed
    | ClientCancelled UTCTime
    | OperatorRejected Text
    | OperatorAccepted Text
    | OperatorArchived Text
    | TimeoutArchieved Text
    deriving (Generic, Show, Read, Eq)
derivePersistField "DepositRequestStatus"

data WithdrawalStatus
    = WsNew
    | WsClientCancelled UTCTime
    | WsOperatorRejected Text
    | WsOperatorExecuted Text
    | WsOperatorArchived Text
    | WsTimeoutArchieved Text
    deriving (Generic, Show, Read, Eq)
derivePersistField "WithdrawalStatus"

instance ToJSON DepositRequestStatus where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON DepositRequestStatus

instance ToJSON WithdrawalStatus where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON WithdrawalStatus

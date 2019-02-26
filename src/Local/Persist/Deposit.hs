module Local.Persist.Deposit where


import Prelude

import Database.Persist.TH

import Data.Text (Text)


data DepositRequestStatus
    = New
    | ClientConfirmed
    | ClientCancelled Text
    | OperatorRejected Text
    | OperatorAccepted Text
    | OperatorExecuted Text
    | OperatorArchived Text
    | TimeoutArchieved Text
    deriving (Show, Read, Eq)
derivePersistField "DepositRequestStatus"
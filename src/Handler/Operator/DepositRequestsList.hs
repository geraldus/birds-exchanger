module Handler.Operator.DepositRequestsList where


import Import
import Local.Persist.Deposit

import Database.Persist.Sql (fromSqlKey)


getOperatorDepositRequestsListR :: Handler Html
getOperatorDepositRequestsListR = do
    requireStaffId
    depositRequestsList <- runDB $ selectList [DepositRequestStatus ==. ClientConfirmed] []
    defaultLayout $(widgetFile "operator/deposit-requests-list")
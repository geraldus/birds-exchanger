{-# LANGUAGE OverloadedStrings #-}
module Handler.Operator.DepositRequestsList where


import           Import
import           Local.Persist.Deposit
import           Utils.Deposit

import           Database.Persist.Sql           ( fromSqlKey
                                                , rawSql
                                                )
import           Data.Text.Format.Numbers       ( prettyF
                                                , PrettyCfg(..)
                                                )


getOperatorDepositRequestsListR :: Handler Html
getOperatorDepositRequestsListR = do
    _    <- requireStaffId
    list <-
        runDB $ rawSql s [toPersistValue ClientConfirmed] :: Handler
            [(Entity DepositRequest, Entity UserWallet, Entity User)]
        -- selectList [DepositRequestStatus ==. ClientConfirmed] []
    defaultLayout $(widgetFile "operator/deposit-requests-list")
  where
    s
        = "SELECT ??, ??, ?? FROM deposit_request, user_wallet, \"user\" \
        \ WHERE deposit_request.status = ? \
        \ AND deposit_request.archived = FALSE \
        \ AND deposit_request.user_id = \"user\".id \
        \ AND user_wallet.currency = deposit_request.currency \
        \ AND user_wallet.user_id = deposit_request.user_id \
        \ ORDER BY deposit_request.created ASC"

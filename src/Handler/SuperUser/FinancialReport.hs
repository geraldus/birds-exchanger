{-# LANGUAGE OverloadedStrings #-}
module Handler.SuperUser.FinancialReport where

import           Import


getSuperUserFinancialReportViewR :: Handler Html
getSuperUserFinancialReportViewR = do
    su <- requireSu
    renderUrl <- getUrlRender
    rm <- getMessageRender
    defaultLayout $ do
        addScriptRemote "https://unpkg.com/react@16/umd/react.development.js"
        addScriptRemote "https://unpkg.com/react-dom@16/umd/react-dom.development.js"
        addScriptAttrs (StaticR js_bundle_js) [("defer","defer")]
        toWidget [julius|window.app = {
            config: {
                su: {
                    socket: #{renderUrl SuperUserWebSocketR}
                        .replace(/^http(s?:\/\/)/, 'ws$1'),
                    labels: {
                        userCount: #{rm MsgClientUsersCount},
                        innerProfit: #{rm MsgFeeStats},
                        activeDeposits: #{rm MsgActiveDepositsCount},
                        acceptedDeposits: #{rm MsgAcceptedDepositsCount},
                        deposit: {
                            income: {
                                realTotal: #{rm MsgDepositIncomeRealTotal},
                                feeTotal: #{rm MsgFee}
                            }
                        },
                        wallets: {
                            balanceTotals: #{rm MsgUserWalletBalancesTotal}
                        }
                    }
                }
            }
        }|]
        [whamlet|<div #react-host>|]

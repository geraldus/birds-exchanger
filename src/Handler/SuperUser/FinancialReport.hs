{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.SuperUser.FinancialReport where

import           Import


getSuperUserFinancialReportViewR :: Handler Html
getSuperUserFinancialReportViewR = do
    requireSu
    renderUrl <- getUrlRender
    rm <- getMessageRender
    let reactBuild =
#ifdef DEVELOPMENT
            "development"
#else
            "production.min"
#endif
    defaultLayout $ do
        addScriptRemote $ "https://unpkg.com/react@16/umd/react." <> reactBuild <> ".js"
        addScriptRemote $ "https://unpkg.com/react-dom@16/umd/react-dom." <> reactBuild <> ".js"
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
                        },
                        withdrawal: {
                            stats: #{rm MsgWithdrawalStatsTitle},
                            new: {
                                count: #{rm MsgWithdrawalAwaitingExecution},
                                amount: #{rm MsgWithdrawalAmountTotal},
                                frozen: #{rm MsgWithdrawalFrozenTotal}
                            },
                            accepted: {
                                count: #{rm MsgWithdrawalAcceptedCount},
                                transfered: #{rm MsgWithdrawalTransferedTotal},
                                fee: #{rm MsgWithdrawalFeeTotal}
                            }
                        }
                    }
                }
            }
        }|]
        [whamlet|<div #react-host>|]

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Form.Profile.Withdrawal where

import           Import
import           Type.Withdrawal
import           Form.Money                     ( moneyInput )
import           Utils.Render                   ( renderFeeAsPct )
import           Utils.Withdrawal               ( defRurWithdrawalFee
                                                , defPzmWithdrawalFee
                                                )

import           Text.Julius                    ( RawJS(..) )


-- TODO: FIXME: Make deposit and withdrawal fees configurable via admin's UI

withdrawalForm :: Text -> Form WithdrawalM
withdrawalForm formId extra = do
    adrid <- newIdent
    amtid <- newIdent
    tmid  <- newIdent
    cid   <- newIdent
    fid   <- newIdent
    (moneyRes         , moneyWid          ) <- moneyInput amtid cid
    (transferMethodRes, transferMethodView) <- mreq
        transferMethodSelect
        (fsAddAttrs [("required", "required")] (fsBs4WithId tmid))
        Nothing
    (addressRes, addressView) <- mreq hiddenField (fsBs4WithId adrid) Nothing
    (feeHiddenRes, feeHiddenView) <- mreq hiddenField (fsBs4WithId fid) Nothing
    let widget = $(widgetFile "form/withdrawal")
        res =
            WithdrawalM
                <$> moneyRes
                <*> transferMethodRes
                <*> feeHiddenRes
                <*> addressRes
    return (res, widget)

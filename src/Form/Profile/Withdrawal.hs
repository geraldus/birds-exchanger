{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Form.Profile.Withdrawal where

import           Form.Money      ( moneyInput )
import           Import
import           Local.Params
import           Type.Withdrawal
import           Utils.Render    ( renderFeeAsPct )

import           Text.Julius     ( RawJS (..) )


-- TODO: FIXME: Make deposit and withdrawal fees configurable via admin's UI

withdrawalForm :: Text -> Form WithdrawalM
withdrawalForm formId extra = do
    projType <- appType . appSettings <$> getYesod
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

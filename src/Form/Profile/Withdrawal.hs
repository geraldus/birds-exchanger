{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Form.Profile.Withdrawal where

import           Import
import           Type.Withdrawal
import           Form.Money                     ( moneyInput )

import           Text.Julius                    ( RawJS(..) )


withdrawalForm :: Text -> Form WithdrawalM
withdrawalForm formId extra = do
    adrid <- newIdent
    amtid <- newIdent
    tid   <- newIdent
    cid   <- newIdent
    (moneyRes         , moneyWid          ) <- moneyInput amtid cid
    (transferMethodRes, transferMethodView) <- mreq
        transferMethodSelect
        (fsAddAttrs [("required", "required")] (fsBs4WithId tid))
        Nothing
    (addressRes, addressView) <- mreq hiddenField (fsBs4WithId adrid) Nothing
    let widget = $(widgetFile "form/withdrawal")
        res    = WithdrawalM <$> moneyRes <*> transferMethodRes <*> addressRes
    return (res, widget)


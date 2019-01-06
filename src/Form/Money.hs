{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Form.Money where


import           Import

import           Type.Money
import           Utils.Money ( truncCoins2Cents )


moneyInput :: Text
           -> Text
           -> MForm (HandlerFor App) (FormResult Money, Widget)
moneyInput aid cid = do
    (amt, av) <- mreq
        doubleField
        (fsAddAttrs
            [ ("min", "0")
            , ("step", "0.01") ]
            (fsAddClasses
                (fsAddPlaceholder (fsBs4WithId aid) "0.00")
                    ["form-control-lg", "text-center"]))
        Nothing
    (cur, cv) <- mreq
        currencySelect
        (fsAddClasses (fsBs4WithId cid) ["form-control-lg"])
        Nothing
    let wid = [whamlet|
                <div .form-row>
                    <div .form-group .col-12 .col-md-4>
                        <label for="#{aid}">сумма
                        ^{fvInput av}
                    <div .form-group .col-12 .col-md-8>
                        <label for="#{cid}">валюта
                        ^{fvInput cv}
                |]
    return  (Money <$> (truncCoins2Cents <$> amt) <*> cur, wid)

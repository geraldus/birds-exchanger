{-# LANGUAGE QuasiQuotes       #-}
module Form.Money where


import           Import

import           Type.Money
import           Utils.Money ( truncCoins2Cents )


moneyInput :: Text
           -> Text
           -> MForm (HandlerFor App) (FormResult Money, Widget)
moneyInput aid cid = do
    (amt, av) <- mreq doubleField (fsBs4WithId aid) Nothing
    (cur, cv) <- mreq currencySelect (fsBs4WithId cid) Nothing
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

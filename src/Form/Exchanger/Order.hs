{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Form.Exchanger.Order where

import           Import

import           Local.Persist.Currency
import           Local.Persist.UserRole
import           Utils.Deposit          (doubleToCents)


-- | This form does not check if user specified valid coins amount.
--   You should do this check in handler yourself
formCreateExchageOrder :: Form ExchangeOrderData
formCreateExchageOrder extra = do
    outIdent <- newIdent
    inIdent <- newIdent
    ratioIdent <- newIdent
    amountIdent <- newIdent
    feeIdent <- newIdent
    (currencyOutRes, currencyOutView) <- mreq currencySelect (bs4InputWithIdFs outIdent) Nothing
    (currencyInRes, currencyInView) <- mreq currencySelect (bs4InputWithIdFs inIdent) Nothing
    (ratioRes, ratioView) <- mreq doubleField (bs4InputWithIdFs ratioIdent) Nothing
    (amountRes, amountView) <- mreq doubleField (bs4InputWithIdFs amountIdent) Nothing
    time <- liftIO getCurrentTime
    let isValidExchangeR = isValidExchange <$> currencyOutRes <*> currencyInRes
        -- haveRequiredAmountR = haveRequiredAmount <$>
    let result = case isValidExchangeR of
            FormSuccess True  -> FormSuccess ExchangeOrderData
            FormSuccess False -> FormFailure ["Неверная пара для обмена"]
            FormFailure es    -> FormFailure es
            FormMissing       -> FormMissing
    let widget = [whamlet|
            <div .form-group>
                <label for="#{outIdent}">обмениваю
                ^{fvInput currencyOutView}
            <div .form-group>
                <label for="#{inIdent}">на
                ^{fvInput currencyOutView}
            <div .form-group>
                <label for="#{ratioIdent}">по цене
                ^{fvInput ratioView}
            <div .form-group>
                <label for="#{amountIdent}">шт.
                ^{fvInput amountView}
            <div .form-group>
                <label for="#{feeIdent}">комиссия
                <input ##{feeIdent} .form-control type=number readonly=readonly value="0">
            |]
    return (result, widget)
    where
        bs4InputWithIdFs ident =
            fsWithClasses "form-control" "" Nothing Nothing Nothing [("id", ident)]
        bs4InputFs = fsWithClasses "form-control" "" Nothing Nothing Nothing []
        fsWithClasses classList lbl tlt mid mnam attrs =
            FieldSettings lbl tlt mid mnam (attrs ++ [("class", classList)])
        -- fs = FieldSettings "some message" Nothing Nothing Nothing [("readonly", "readonly")]



isValidExchange :: Currency -> Currency -> Bool
isValidExchange (FiatC RUR) (CryptoC PZM) = True
isValidExchange (CryptoC PZM) (FiatC RUR) = True
isValidExchange _ _                       = False



data ExchangeOrderData = ExchangeOrderData

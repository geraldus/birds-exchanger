{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Form.Exchanger.Order where

import           Import

import           Local.Persist.Currency
import Type.Fee (Fee(..))
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
    (currencyInRes, currencyInView)   <- mreq currencySelect (bs4InputWithIdFs inIdent) Nothing
    (ratioRes, ratioView)             <- mreq doubleField (bs4InputWithIdFs ratioIdent) Nothing
    (amountRes, amountView)           <- mreq doubleField (bs4InputWithIdFs amountIdent) Nothing
    let isValidExchangeR = isValidExchange <$> currencyOutRes <*> currencyInRes
        amountCentsRes   = doubleToCents   <$> amountRes
        feeRes           = targetFeeC      <$> amountCentsRes <*> ratioRes
    let result = case isValidExchangeR of
            FormSuccess True  -> ExchangeOrderData
                    <$> currencyOutRes
                    <*> currencyInRes
                    <*> ratioRes
                    <*> amountCentsRes
                    <*> feeRes
            FormSuccess False -> FormFailure ["Неверная пара для обмена"]
            FormFailure es    -> FormFailure es
            FormMissing       -> FormMissing
    let widget = [whamlet|
            #{extra}
            <div .form-group>
                <label for="#{outIdent}">обмениваю
                ^{fvInput currencyOutView}
            <div .form-group>
                <label for="#{inIdent}">на
                ^{fvInput currencyInView}
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


isValidExchange :: Currency -> Currency -> Bool
isValidExchange (FiatC RUR) (CryptoC PZM) = True
isValidExchange (CryptoC PZM) (FiatC RUR) = True
isValidExchange _ _                       = False


targetFeeC :: Int -> Double -> Int
targetFeeC amt ratio = case defaultExchangeFee of
    Percent p -> truncate $ fromIntegral amt * ratio * p / 100
    CentsFixed c -> c


data ExchangeOrderData = ExchangeOrderData
    { orderDataCurrencyOut      :: Currency
    , orderDataCurrencyIn       :: Currency
    , orderDataRatio            :: Double
    , orderDataAmountCents      :: Int
    , orderDataExpectedFeeCents :: Int
    }
    deriving Show

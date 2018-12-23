{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Form.Exchanger.Order where

import           Import

import           Local.Persist.Currency
import           Local.Persist.ExchangeOrder ( ExchangePair (..) )
import           Type.Fee                    ( Fee (..) )
import           Utils.Deposit               ( doubleToCents, oneCoinCents )

import           Text.Julius                 ( RawJS (..) )


-- | This form does not check if user specified valid coins amount.
--   You should do this check in handler yourself
-- formCreateExchageOrder :: Form ExchangeOrderData
-- formCreateExchageOrder extra = do
--     outIdent <- newIdent
--     inIdent <- newIdent
--     ratioIdent <- newIdent
--     amountIdent <- newIdent
--     feeIdent <- newIdent
--     (currencyOutRes, currencyOutView) <- mreq currencySelect (fsBs4WithId outIdent) Nothing
--     (currencyInRes, currencyInView)   <- mreq currencySelect (fsBs4WithId inIdent) Nothing
--     (ratioRes, ratioView)             <- mreq doubleField    (fsBs4WithId ratioIdent) Nothing
--     (amountRes, amountView)           <- mreq doubleField    (fsBs4WithId amountIdent) Nothing
--     let isValidExchangeR = isValidExchange <$> currencyOutRes <*> currencyInRes
--         amountCentsRes   = doubleToCents   <$> amountRes
--         feeRes           = targetFeeC      <$> amountCentsRes <*> ratioRes
--     let result = case isValidExchangeR of
--             FormSuccess True  -> ExchangeOrderData
--                     <$> currencyOutRes
--                     <*> currencyInRes
--                     <*> ratioRes
--                     <*> amountCentsRes
--                     <*> feeRes
--             FormSuccess False -> FormFailure ["Неверная пара для обмена"]
--             FormFailure es    -> FormFailure es
--             FormMissing       -> FormMissing
--     let widget = [whamlet|
--             #{extra}
--             <div .form-group>
--                 <label for="#{outIdent}">обмениваю
--                 ^{fvInput currencyOutView}
--             <div .form-group>
--                 <label for="#{inIdent}">на
--                 ^{fvInput currencyInView}
--             <div .form-group>
--                 <label for="#{ratioIdent}">по цене
--                 ^{fvInput ratioView}
--             <div .form-group>
--                 <label for="#{amountIdent}">шт.
--                 ^{fvInput amountView}
--             <div .form-group>
--                 <label for="#{feeIdent}">сервисный сбор
--                 <input ##{feeIdent} .form-control type=number readonly=readonly value="0">
--             |]
--     return (result, widget)


createOrderForm :: ExchangePair -> Form OrderFD
createOrderForm defaultPair extra = do
    (wrapid:actid:amtid:ratid:sumid:feeid:pairid:_) <- mapM (\_ -> newIdent) [ 1..8 ]
    (actionRes, actionView) <- mreq
        actionField
        ( fsAddClasses
            ( fsBs4WithId actid )
            ( fsOpts <> [  "font-weight-bold" ] ) )
        Nothing
    (amountRes, amountView) <- mreq
        doubleField
        ( fsAddClasses
            ( fsAddPlaceholder (fsBs4WithId amtid) "кол-во" )
            ( fsOpts <> [  "font-weight-bold" ] ) )
        Nothing
    (ratioRes, ratioView) <- mreq
        doubleField
        ( fsAddAttrs [ ( "autocomplete", "off" ) ]
            ( fsAddClasses
                ( fsAddPlaceholder (fsBs4WithId ratid) "67.00" )
                fsOpts ) )
        Nothing
    (feeRes, hiddenFeeView) <- mreq hiddenField (fsBs4WithId feeid) (Nothing :: Maybe Int)
    (pairRes, hiddenPairView) <- mreq hiddenField (fsBs4WithId pairid) (Just defaultPair)
    let result = OrderFD
            <$> actionRes
            <*> (fmap doubleToCents amountRes)
            <*> ratioRes
            <*> feeRes
            <*> pairRes
        (Percent feePercent) = defaultExchangeFee
        widget = $(widgetFile "form/create-order")
    return (result, widget)
    where
        actionField = selectField $
            optionsPairs [ ("Отдаю" :: SomeMessage App, EAGive)
                         , ("Принимаю", EAReceive)]
        fsOpts =
            [ "form-control-lg"
            , "text-right"
            , "bg-dark"
            , "text-white" ]

data OrderFD = OrderFD
    { action :: ExchangeAction
    , amount :: Int
    , ratio  :: Double
    , fee    :: Int
    , pair   :: ExchangePair
    }
    deriving Show

data ExchangeAction = EAGive | EAReceive
    deriving (Show, Eq)

isValidExchange :: Currency -> Currency -> Bool
isValidExchange (FiatC RUR) (CryptoC PZM) = True
isValidExchange (CryptoC PZM) (FiatC RUR) = True
isValidExchange _ _                       = False


targetFeeC :: Int -> Double -> Int
targetFeeC amt ratio = case defaultExchangeFee of
    CentsFixed c -> c
    Percent    p -> truncate $
        fromIntegral amt * ratio * p / fromIntegral oneCoinCents


data ExchangeOrderData = ExchangeOrderData
    { orderDataCurrencyOut      :: Currency
    , orderDataCurrencyIn       :: Currency
    , orderDataRatio            :: Double
    , orderDataAmountCents      :: Int
    , orderDataExpectedFeeCents :: Int
    }
    deriving Show

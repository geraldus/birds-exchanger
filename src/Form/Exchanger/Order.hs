{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Form.Exchanger.Order where

import           Import

import           Local.Persist.Currency
import           Local.Persist.ExchangeOrder    ( ExchangePair(..) )
import           Type.Fee                       ( Fee(..) )
import           Utils.Money                    ( truncCoins2Cents )
import           Type.Money                     ( oneCoinCents )

import           Text.Julius                    ( RawJS(..) )


createOrderForm :: ExchangePair -> Form OrderFD
createOrderForm defaultPair extra = do
    (wrapid : actid : amtid : ratid : sumid : feeid : pairid : _) <- mapM
        (const newIdent)
        [(1 :: Integer) .. 8]
    (actionRes, actionView) <- mreq
        actionField
        (fsAddClasses (fsBs4WithId actid) (fsOpts <> ["font-weight-bold"]))
        Nothing
    (amountRes, amountView) <- mreq
        doubleField
        (fsAddClasses (fsAddPlaceholder (fsBs4WithId amtid) "кол-во")
                      (fsOpts <> ["font-weight-bold"])
        )
        Nothing
    (ratioRes, ratioView) <- mreq
        doubleField
        (fsAddAttrs
            [("autocomplete", "off")]
            (fsAddClasses (fsAddPlaceholder (fsBs4WithId ratid) "67.00") fsOpts)
        )
        Nothing
    (feeRes, hiddenFeeView) <- mreq hiddenField
                                    (fsBs4WithId feeid)
                                    (Nothing :: Maybe Int)
    (pairRes, hiddenPairView) <- mreq hiddenField
                                      (fsBs4WithId pairid)
                                      (Just defaultPair)
    let result =
            OrderFD
                <$> actionRes
                <*> fmap truncCoins2Cents amountRes
                <*> ratioRes
                <*> feeRes
                <*> pairRes
        (Percent feePercent) = defaultExchangeFee
        widget               = $(widgetFile "form/create-order")
    return (result, widget)
  where
    actionField = selectField $ optionsPairs
        [("Отдаю" :: SomeMessage App, EAGive), ("Принимаю", EAReceive)]
    fsOpts = ["form-control-lg", "text-right", "bg-dark", "text-white"]

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
isValidExchange (FiatC   RUR) (CryptoC PZM) = True
isValidExchange (CryptoC PZM) (FiatC   RUR) = True
isValidExchange _             _             = False


targetFeeC :: Int -> Double -> Int
targetFeeC amt ratio = case defaultExchangeFee of
    CentsFixed c -> c
    Percent p ->
        truncate $ fromIntegral amt * ratio * p / fromIntegral oneCoinCents


data ExchangeOrderData = ExchangeOrderData
    { orderDataCurrencyOut      :: Currency
    , orderDataCurrencyIn       :: Currency
    , orderDataRatio            :: Double
    , orderDataAmountCents      :: Int
    , orderDataExpectedFeeCents :: Int
    }
    deriving Show

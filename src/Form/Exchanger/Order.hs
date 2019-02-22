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


createOrderForm :: Text -> ExchangePair -> Form OrderFD
createOrderForm ratid defaultPair extra = do
    wrapid                  <- newIdent
    actid                   <- newIdent
    amtid                   <- newIdent
    sumid                   <- newIdent
    feeid                   <- newIdent
    pairid                  <- newIdent
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

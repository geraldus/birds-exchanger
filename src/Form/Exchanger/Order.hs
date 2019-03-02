{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Form.Exchanger.Order where

import           Import

import           Local.Persist.Exchange ( ExchangePair (..) )
import           Type.Fee               ( Fee (..) )
import           Utils.Money            ( truncCoins2Cents )

import           Text.Julius            ( RawJS (..) )


createOrderForm :: Text -> Text -> ExchangePair -> Form OrderFD
createOrderForm wrapId ratid defaultPair extra = do
    actid  <- newIdent
    amtid  <- newIdent
    sumid  <- newIdent
    feeid  <- newIdent
    pairid <- newIdent
    let wrapid = wrapId
    (actionRes, actionView) <- mreq
        actionField
        (fsAddClasses
            (fsBs4WithId actid)
            (fsOpts <> ["font-weight-bold", "exchange-action-input"])
        )
        Nothing
    (amountRes, amountView) <- mreq
        doubleField
        (fsAddAttrs
            [("min", "0")]
            (fsAddClasses (fsAddPlaceholder (fsBs4WithId amtid) "кол-во")
                      (fsOpts <> ["font-weight-bold", "amount-input"])
            )
        )
        Nothing
    (ratioRes, ratioView) <- mreq
        doubleField
        (fsAddAttrs
            [("autocomplete", "off")
            ,("min", "0")]
            (fsAddClasses (fsAddPlaceholder (fsBs4WithId ratid) "67.00")
                          (fsOpts <> ["ratio-input"])
            )
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

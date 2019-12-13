{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Form.Exchanger.Order where

import           Import

import           Local.Params           ( defaultExchangeFee )
import           Local.Persist.Currency ( Currency (..), currencySymbol )
import           Local.Persist.Exchange ( ExchangePair (..) )
import           Type.Fee               ( Fee (..) )
import           Utils.Money            ( truncCoins2Cents, unPairCurrency )

import           Text.Julius            ( RawJS (..) )


createOrderForm :: Text -> Text -> ExchangePair -> Form OrderFD
createOrderForm wrapId ratid defaultPair extra = do
    userWallets <- lift maybeClient >>= return . maybe [] snd
    actid  <- newIdent
    amtid  <- newIdent
    sumid  <- newIdent
    let wrapid = wrapId
        feeid = wrapId <> "-hidden-fee"
        pairid = wrapId <> "-hidden-pair"
    (actionRes, actionView) <- mreq
        actionField
        (fsAddClasses
            (fsBs4WithId actid)
            (fsOpts <> ["font-weight-bold", "exchange-action-input"])
        )
        Nothing
    (pairRes, hiddenPairView) <- mreq
            hiddenField (fsBs4WithId pairid) (Just defaultPair)
    (amountRes, amountView) <- mreq
            doubleField
            (fsAddAttrs
                [("min", "0")]
                (fsAddClasses (fsAddPlaceholder (fsBs4WithId amtid) "кол-во")
                        (fsOpts <> ["amount-input"])
                )
             )
            Nothing
    (ratioRes, ratioView) <- mreq
        doubleField
        (fsAddAttrs
            [("autocomplete", "off")
            ,("min", "0")]
            (fsAddClasses (fsAddPlaceholder (fsBs4WithId ratid) "курс")
                          (fsOpts <> ["ratio-input"])
            )
        )
        Nothing
    (feeRes, hiddenFeeView) <- mreq
            hiddenField (fsBs4WithId feeid) (Nothing :: Maybe Int)
    renderMessage' <- getMessageRender
    let validate = validateParams renderMessage' userWallets
        validatedAmountRes = joinResult
            (validate <$> pairRes <*> actionRes  <*> amountRes)
    let result =
            OrderFD
                <$> actionRes
                <*> fmap truncCoins2Cents validatedAmountRes
                <*> ratioRes
                <*> feeRes
                <*> pairRes
        (Percent feePercent) = defaultExchangeFee
        (currencyOut, currencyIn) = unPairCurrency defaultPair
    let widget               = $(widgetFile "form/create-order")
    return (result, widget)
  where
    actionField = selectField $ optionsPairs
        [("Отдаю" :: SomeMessage App, EAGive), ("Принимаю", EAReceive)]
    fsOpts = ["form-control-lg", "text-right", "bg-dark", "text-white"]

    validateParams
        :: (AppMessage -> Text)
        -> [Entity UserWallet]
        -> ExchangePair
        -> ExchangeAction
        -> Double
        -> FormResult Double
    validateParams mr ws p EAGive a =
            validateByCurrency mr ws (fst $ unPairCurrency p) a
    validateParams mr ws p EAReceive a =
            validateByCurrency mr ws (snd $ unPairCurrency p) a

    validateByCurrency :: (AppMessage -> Text) -> [Entity UserWallet] -> Currency -> Double -> FormResult Double
    validateByCurrency renderMessage' [] _ _ =
        FormFailure [ renderMessage' MsgFormMessageErrorUserWalletNotFound ]
    validateByCurrency renderMessage' (Entity _ w:ws) c amt
        | userWalletCurrency w == c
            = if fromIntegral (userWalletAmountCents w) >= amt * 100
                then FormSuccess amt
                else FormFailure [ renderMessage' MsgFormMessageErrorNotEnoughFunds ]
        | otherwise = validateByCurrency renderMessage' ws c amt

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


joinResult :: FormResult (FormResult a) -> FormResult a
joinResult FormMissing                    = FormMissing
joinResult (FormFailure es)               = FormFailure es
joinResult (FormSuccess FormMissing)      = FormMissing
joinResult (FormSuccess (FormFailure es)) = FormFailure es
joinResult (FormSuccess (FormSuccess r))  = FormSuccess r

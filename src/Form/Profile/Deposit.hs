{-# LANGUAGE OverloadedStrings #-}
module Form.Profile.Deposit where


import           Import


depositForm :: Form (FormResult DepositRequestFD, Widget)
depositForm extra = do
    (paymentCurrencyRes, paymentCurrencyView) <- mreq currencySelect "" Nothing
    (paymentCurrencyAmountRes, paymentCurrencyAmountView) <- mreq doubleField "" Nothing
    (paymentMethodRes, paymentMethodView) <- mreq (selectFieldList paymentMethodOptions) "" Nothing
    (targetCurrencyRes, targetCurrencyView) <- mreq (selectFieldList currencyOptions) "" Nothing

    -- let depReqRes = DepositRequestFD
    --         <$> payment PaymentMethod Int Currency
    let widget = do
            inCurrencyId <- newIdent
            inTargetCurrencyId <- newIdent
            [whamlet|
                #{extra}
                <div ##{inCurrencyId}>
                    ^{fvInput paymentCurrencyView}
                <div ##{inTargetCurrencyId}>
                    ^{fvInput targetCurrencyView}
                <div>
                    ^{fvInput paymentCurrencyAmountView}
                ^{fvInput paymentMethodView}
                |]
    return (FormMissing, widget)


data DepositRequestFD = DepositRequestFD
    { depReqCurrency       :: Currency
    , depReqPaymentMethod  :: PaymentMethod
    , depReqCentsAmount    :: Int
    -- , depReqCentsExpectedFee :: Int
    -- , depReqTransactionCode :: Text
    , depReqTargetCurrency :: Currency
    -- , depReqExpectedConversionRatio :: Double
    -- , depReqCentsExpectedIncome :: Amount
    }


currencySelect = selectFieldList currencyOptions

currencyOptions :: [(Text, Currency)]
currencyOptions =
    [ ("₽ российский рубль", FiatC RUR)
    , ("PZM криптовалюта Prizm", CryptoC PZM) ]

paymentMethodOptions :: [(Text, PaymentMethod)]
paymentMethodOptions =
    [ ("Перевод с карты на карту, ₽ российский рубль", FiatPM (FiatPaymentMethod "card2card") RUR)
    , ("Перевод криптовалюты, PZM криптовалюта Prizm", CryptoPM PZM) ]

typedCurrencyOptions :: [(CurrencyType, Currency)]
typedCurrencyOptions =
    [ (FiatT, FiatC RUR)
    , (CryptoT, CryptoC PZM)
    ]



data Currency
    = FiatC FiatCurrency
    | CryptoC CryptoCurrency
    deriving Eq

data CurrencyType
    = FiatT
    | CryptoT
    deriving Eq

data FiatCurrency
    = RUR
    | USD
    deriving Eq

data CryptoCurrency
    = BTC
    | ETH
    | PZM
    deriving Eq

data PaymentMethod
    = FiatPM FiatPaymentMethod FiatCurrency
    | CryptoPM CryptoCurrency
    deriving Eq

data FiatPaymentMethod = FiatPaymentMethod Text deriving Eq

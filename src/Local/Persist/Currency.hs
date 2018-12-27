module Local.Persist.Currency where


import Database.Persist.TH
import Prelude


data CurrencyType
    = FiatT
    | CryptoT
    deriving (Show, Read, Eq)
derivePersistField "CurrencyType"

data FiatCurrency
    = RUR
    | USD
    deriving (Show, Read, Eq)
derivePersistField "FiatCurrency"

data CryptoCurrency
    = PZM
    | ETH
    | BTC
    deriving (Show, Read, Eq)
derivePersistField "CryptoCurrency"


data Currency
    = FiatC FiatCurrency
    | CryptoC CryptoCurrency
    deriving (Show, Read, Eq)
derivePersistField "Currency"

pzmC :: Currency
pzmC = CryptoC PZM

rurC :: Currency
rurC = FiatC RUR


data FiatPaymentMethod
    = SberBankCard2CardFPM
    | AlphaBankCard2CardFPM
    | TinkoffBankCard2CardFPM
    | PayPalTransferFPM
    | QiwiFPM
    deriving (Show, Read, Eq)
derivePersistField "FiatPaymentMethod"

data PaymentMethod
    = FiatPM FiatPaymentMethod FiatCurrency
    | CryptoPM CryptoCurrency
    deriving (Show, Read, Eq)
derivePersistField "PaymentMethod"

pmSber :: FiatCurrency -> PaymentMethod
pmSber = FiatPM SberBankCard2CardFPM

pmAlpha :: FiatCurrency -> PaymentMethod
pmAlpha = FiatPM AlphaBankCard2CardFPM

pmTinkoff :: FiatCurrency -> PaymentMethod
pmTinkoff = FiatPM TinkoffBankCard2CardFPM

pmQiwi :: FiatCurrency -> PaymentMethod
pmQiwi = FiatPM QiwiFPM

pmPayPal :: FiatCurrency -> PaymentMethod
pmPayPal = FiatPM PayPalTransferFPM

fpmSberRur :: PaymentMethod
fpmSberRur = pmSber RUR

fpmAlphaRur :: PaymentMethod
fpmAlphaRur = pmAlpha RUR

fpmTinkoffRur :: PaymentMethod
fpmTinkoffRur = pmTinkoff RUR

fpmQiwiRur :: PaymentMethod
fpmQiwiRur = pmQiwi RUR

fpmPayPalRur :: PaymentMethod
fpmPayPalRur = pmPayPal RUR

fpmPayPalUsd :: PaymentMethod
fpmPayPalUsd = pmPayPal USD

cpmPzm :: PaymentMethod
cpmPzm = CryptoPM PZM


cpmBtc :: PaymentMethod
cpmBtc = CryptoPM BTC


cpmEth :: PaymentMethod
cpmEth = CryptoPM ETH

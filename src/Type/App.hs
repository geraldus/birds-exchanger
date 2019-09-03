{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Type.App where

import           Import.NoFoundation
import           Local.Persist.Currency

data MenuItem m where
    MenuItem :: RenderRoute m =>
        { menuItemLabel          :: Text
        , menuItemRoute          :: Route m
        , menuItemAccessCallback :: Bool
        } -> MenuItem m

data MenuTypes m
    = NavbarLeft (MenuItem m)
    | NavbarRight (MenuItem m)


type TransactionsCount = Int

type PaymentAddressee = [Text]

data PaymentAddress = PaymentAddress
    { paymentAddressAddressee :: PaymentAddressee
    , paymentAddressCount     :: Int }

data PaymentMethod
    = FiatPaymentMethod TransferMethod TransactionsCount [PaymentAddress]
    | CryptoPaymentMethod CryptoCurrency TransactionsCount [PaymentAddress]


data AppPaymentMethods = AppPaymentMethods
    { appDepositFiatMethods   :: [PaymentMethod]
    , appDepositCryptoMethods :: [PaymentMethod]
    }

hardcodedPaymentMethods :: AppPaymentMethods
hardcodedPaymentMethods = AppPaymentMethods
    { appDepositFiatMethods =
        [ FiatPaymentMethod
            (FiatTM SberBankCard2CardFTM RUR)
            0
            [ PaymentAddress ["5469 7200 1233 4856"] 0 ]
        , FiatPaymentMethod
            (FiatTM TinkoffBankCard2CardFTM RUR)
            0
            [ PaymentAddress ["5536 9137 9648 0594"] 0 ]
        , FiatPaymentMethod
            (FiatTM QiwiFTM RUR)
            0
            [ PaymentAddress ["+79090991177"] 0 ]
        ]
    , appDepositCryptoMethods =
        [ CryptoPaymentMethod
            PZM
            0
            [ PaymentAddress
                [ "PRIZM-8GBY-JZ9V-UJAZ-DNLU2"
                , "12d9435fa9a3ecf3c11c6b8bf7662dec44842616d2cde82cfbf8fb489b3d6d16" ]
                0 ]
        , CryptoPaymentMethod
            BTC
            0
            [ PaymentAddress ["1Hih1ccN7oAxfYWh2tTENiesJ69vt8fdvS"] 0 ]
        , CryptoPaymentMethod
            ETH
            0
            [ PaymentAddress ["1Hih1ccN7oAxfYWh2tTENiesJ69vt8fdvS"] 0 ]
        , CryptoPaymentMethod
            OUR
            0
            [ PaymentAddress
                ["TEST ADDRESS"] 0 ] ]
    }

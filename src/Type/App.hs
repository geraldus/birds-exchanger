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
    deriving Show

data PaymentMethod
    = FiatPaymentMethod TransferMethod TransactionsCount [PaymentAddress]
    | CryptoPaymentMethod CryptoCurrency TransactionsCount [PaymentAddress]
    deriving Show

data AppPaymentMethods = AppPaymentMethods
    { appDepositFiatMethods   :: [PaymentMethod]
    , appDepositCryptoMethods :: [PaymentMethod]
    }
    deriving Show

hardcodedPaymentMethods :: AppPaymentMethods
hardcodedPaymentMethods = AppPaymentMethods
    { appDepositFiatMethods =
        [ FiatPaymentMethod
            (FiatTM SberBankCard2CardFTM RUR)
            0
            [ PaymentAddress ["5469 7200 1130 6541"] 0 ]
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
                ["ouro1uru3e8tfudnh7usswjqegquvt9scw2aw5q7w6z"] 0 ] ]
    }

defaultPaymentAddressRotationThreshold :: Int
defaultPaymentAddressRotationThreshold = 10

defaultSelectNextAddr :: PaymentMethod -> (Maybe PaymentAddress, PaymentMethod)
defaultSelectNextAddr pm@(FiatPaymentMethod  _ _ []) = (Nothing, pm)
defaultSelectNextAddr pm@(CryptoPaymentMethod _ _ []) = (Nothing, pm)
defaultSelectNextAddr pm =
    let addrs = getParams pm
        x1 = zip addrs [0..]
        x2 = flip map x1 $ \ (x@(PaymentAddress _ q), n) ->
                    ( x
                    , q `div` defaultPaymentAddressRotationThreshold
                    , n
                    )
        x3 = sortOn ( \(_, snd3, _) -> snd3) x2
        (adr@(PaymentAddress a cnt), _, i) = case x3 of
            h:_ -> h
            _ -> error "Impossible.  Payment address list should be already pattern matched"
        upd = PaymentAddress a (cnt + 1)
        (prev, _:rest) = splitAt i addrs
    in (Just adr, updateMethod pm (prev <> [upd] <> rest))
    where
        getParams (FiatPaymentMethod _ _ as) = as
        getParams (CryptoPaymentMethod _ _ as) = as
        updateMethod (FiatPaymentMethod method count _) addrs
            = FiatPaymentMethod method (count + 1) addrs
        updateMethod (CryptoPaymentMethod cur count _) addrs
            = CryptoPaymentMethod cur (count + 1) addrs

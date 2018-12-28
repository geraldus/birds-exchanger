{-# LANGUAGE OverloadedStrings #-}
module Handler.Client.DepositConfirm where

import Import

import Local.Persist.Deposit
import Local.Persist.Currency


getDepositRequestConfirmationR :: Text -> Handler Html
getDepositRequestConfirmationR code = withRequest' code $ \(Entity _ t) -> do
    when (depositRequestStatus t /= New) $ do
        setMessage "Данная заявка проходит проверку..."
        redirect HomeR
    let transactionCode = depositRequestTransactionCode t
        paymentMethod = depositRequestPaymentMethod t
        cents = depositRequestCentsAmount t
        currency = depositRequestCurrency t
        paymentGuide = case paymentMethod of
            FiatPM SberBankCard2CardFPM RUR -> paymentGuideCard2Card paymentMethod cents currency code
            FiatPM TinkoffBankCard2CardFPM RUR -> paymentGuideCard2Card paymentMethod cents currency code
            FiatPM QiwiFPM RUR -> paymentGuideQiwi paymentMethod cents currency code
            CryptoPM PZM -> paymentGuideCryptoC paymentMethod cents currency code
            _ -> [whamlet|Приём средст временно приостановлен.  Попробуйте позже|]
    defaultLayout $(widgetFile "client/deposit-proceed")


  where
    guideTemplate :: Text -> Int -> Currency -> Text -> Html -> Text -> Widget
    guideTemplate title cents curr desc addr code = [whamlet|
        <p .lead .text-center>#{title}
        <p>Переведите #
            <span .text-monospace.font-weight-bold>#{cents2dblT cents}
            \#{currSign curr}
            \ #{desc} #
            <span .font-weight-bold .text-monospace>#{addr}
        <p>В комментарии платежа #
            <span .font-weight-bold>ОБЯЗАТЕЛЬНО УКАЖИТЕ КОД ОПЕРАЦИИ!
        <div .alert .alert-warning>
            <p .text-center>
                <small>КОД ОПЕРАЦИИ
            <div .text-monospace .text-center .h3>#{code}|]
    paymentGuideCard2Card pm cents curr code =
        guideTemplate (paymentTitle pm) cents curr "на карту" (paymentAddr pm) code
    paymentGuideQiwi pm cents curr code =
        guideTemplate (paymentTitle pm) cents curr "на Qiwi-кошелёк" (paymentAddr pm) code
    paymentGuideCryptoC pm cents curr code =
        let c = case pm of
                CryptoPM c' -> c'
                _ -> error "Ошибка.  Неверные данные"
        in guideTemplate (paymentTitle pm) cents curr (cryptoCDesc c) (paymentAddr pm) code
    paymentTitle :: PaymentMethod -> Text
    paymentTitle (FiatPM SberBankCard2CardFPM _) = "Пополнение переводом на карту СберБанка"
    paymentTitle (FiatPM TinkoffBankCard2CardFPM _) = "Пополнение переводом на карту Тинькофф Банка"
    paymentTitle (FiatPM QiwiFPM _) = "Перевод на Qiwi кошелёк по номеру телефона"
    paymentTitle (CryptoPM curr) = "Перевод на " <> cryptoName curr <> " кошелёк"
    paymentAddr :: PaymentMethod -> Html
    paymentAddr (FiatPM SberBankCard2CardFPM RUR) = "5469 7200 1260 8192"
    paymentAddr (FiatPM TinkoffBankCard2CardFPM RUR) = "5536 9137 9169 3324"
    paymentAddr (FiatPM QiwiFPM RUR) = "+79090991177"
    paymentAddr (CryptoPM PZM) = [shamlet|
        <br>
        PRIZM-8GBY-JZ9V-UJAZ-DNLU2
        <br>
        12d9435fa9a3ecf3c11c6b8bf7662dec44842616d2cde82cfbf8fb489b3d6d16
        |]
    paymentAddr (CryptoPM BTC) = [shamlet|1Hih1ccN7oAxfYWh2tTENiesJ69vt8fdvS|]
    paymentAddr (CryptoPM ETH) = [shamlet|0x790d1e80934232e16FEA0360Ad8963E04Ab528Dc|]
    cryptoCDesc curr = "на " <> cryptoName curr <> " кошелёк"
    cryptoName PZM = "Prizm"
    cryptoName BTC = "Bitcoin"
    cryptoName ETH = "Etherium"


postDepositConfirmRequestR :: Handler Html
postDepositConfirmRequestR = do
    code <- runInputPost $ ireq textField "transaction-code"
    withRequest' code $ \(Entity tid t) -> do
        runDB $ update tid [DepositRequestStatus =. ClientConfirmed]
        setMessage "Заявка проходит проверку"
        redirect HomeR


withRequest'
    :: Text
    -> (Entity DepositRequest -> Handler Html)
    -> Handler Html
withRequest' code action =
    requireClientId >> withRequest code action notFound

withRequest
    :: Text
    -> (Entity DepositRequest -> Handler Html)
    -> Handler Html
    -> Handler Html
withRequest code action fallback = do
    mdr <- runDB . getBy $ UniqueDepositRequest code
    case mdr of
        Nothing -> fallback
        Just e -> action e

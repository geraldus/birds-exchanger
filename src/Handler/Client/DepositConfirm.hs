{-# LANGUAGE OverloadedStrings #-}
module Handler.Client.DepositConfirm where

import           Import

import           Local.Persist.Currency
import           Local.Persist.Wallet   ( DepositRequestStatus (..) )


getDepositRequestConfirmationR :: Text -> Handler Html
getDepositRequestConfirmationR code = withRequest' code $ \(Entity _ t) -> do
    when (depositRequestStatus t /= New) $ do
        setMessage "Данная заявка проходит проверку..."
        redirect HomeR
    let transactionCode = depositRequestTransactionCode t
        transferMethod = depositRequestTransferMethod t
        cents = depositRequestCentsAmount t
        currency = depositRequestCurrency t
        paymentGuide = case transferMethod of
            FiatTM SberBankCard2CardFTM RUR -> paymentGuideCard2Card transferMethod cents currency code
            FiatTM TinkoffBankCard2CardFTM RUR -> paymentGuideCard2Card transferMethod cents currency code
            FiatTM QiwiFTM RUR -> paymentGuideQiwi transferMethod cents currency code
            CryptoTM PZM -> paymentGuideCryptoC transferMethod cents currency code
            _ -> [whamlet|Приём средств временно приостановлен.  Попробуйте позже|]
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
    paymentGuideCard2Card tm cents curr code =
        guideTemplate (paymentTitle tm) cents curr "на карту" (paymentAddr tm) code
    paymentGuideQiwi tm cents curr code =
        guideTemplate (paymentTitle tm) cents curr "на Qiwi-кошелёк" (paymentAddr tm) code
    paymentGuideCryptoC tm cents curr code =
        let c = case tm of
                CryptoTM c' -> c'
                _           -> error "Ошибка.  Неверные данные"
        in guideTemplate (paymentTitle tm) cents curr (cryptoCDesc c) (paymentAddr tm) code
    paymentTitle :: TransferMethod -> Text
    paymentTitle (FiatTM SberBankCard2CardFTM _) = "Пополнение переводом на карту СберБанка"
    paymentTitle (FiatTM TinkoffBankCard2CardFTM _) = "Пополнение переводом на карту Тинькофф Банка"
    paymentTitle (FiatTM QiwiFTM _) = "Перевод на Qiwi кошелёк по номеру телефона"
    paymentTitle (CryptoTM curr) = "Перевод на " <> cryptoName curr <> " кошелёк"
    paymentAddr :: TransferMethod -> Html
    paymentAddr (FiatTM SberBankCard2CardFTM RUR) = "5469 7200 1260 8192"
    paymentAddr (FiatTM TinkoffBankCard2CardFTM RUR) = "5536 9137 9648 0594"
    paymentAddr (FiatTM QiwiFTM RUR) = "+79090991177"
    paymentAddr (CryptoTM PZM) = [shamlet|
        <br>
        PRIZM-8GBY-JZ9V-UJAZ-DNLU2
        <br>
        12d9435fa9a3ecf3c11c6b8bf7662dec44842616d2cde82cfbf8fb489b3d6d16
        |]
    paymentAddr (CryptoTM BTC) = [shamlet|1Hih1ccN7oAxfYWh2tTENiesJ69vt8fdvS|]
    paymentAddr (CryptoTM ETH) = [shamlet|0x790d1e80934232e16FEA0360Ad8963E04Ab528Dc|]
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
        redirect DepositR

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
        Just e  -> action e

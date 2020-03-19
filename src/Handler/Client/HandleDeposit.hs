{-# LANGUAGE OverloadedStrings #-}
module Handler.Client.HandleDeposit where

import           Import

import           Local.Persist.Currency
import           Local.Persist.TransferMethod ( FiatTransferMethod (..),
                                                TransferMethod (..) )
import           Local.Persist.Wallet         ( DepositRequestStatus (..) )
import           Utils.Money

import           Data.Aeson                   ( decode )
import           Database.Persist.Sql         ( fromSqlKey, toSqlKey )

{-
Qiwкарты:
сбер 5469 7200 1233 4856
все остальное остается.
позже добавим еще тинькоф

  +79090991177

Tinkoff
  5536 9137 9169 3324
  5536 9137 9648 0594 Pavel Koval
  5536 9137 9788 6542 Pavel Golenkov

PRIZM
  PRIZM-8GBY-JZ9V-UJAZ-DNLU2
  12d9435fa9a3ecf3c11c6b8bf7662dec44842616d2cde82cfbf8fb489b3d6d16

Bitcoin
  1Hih1ccN7oAxfYWh2tTENiesJ69vt8fdvS

Etherium
  0x790d1e80934232e16FEA0360Ad8963E04Ab528Dc

Устаревшее
Sber
  5469 7200 1260 8192

  -}


getDepositRequestConfirmationR :: Text -> Handler Html
getDepositRequestConfirmationR code = withClientRequestByCode code $
    \(Entity tid t) -> do
        when (depositRequestStatus t /= New) $ do
            setMessageI MsgDepositRequestAlreadyConfirmed
            redirect HomeR
        let transactionId = tid
            -- transactionCode = depositRequestTransactionCode t
            transferMethod = depositRequestTransferMethod t
            transferAddressee = fromMaybe
                    []
                    ((decode . fromStrict . encodeUtf8) (depositRequestAddressee t))
            cents = depositRequestCentsAmount t
            currency = depositRequestCurrency t
            paymentGuide = case transferMethod of
                FiatTM SberBankCard2CardFTM RUB -> paymentGuideCard2Card transferMethod transferAddressee cents currency code
                FiatTM TinkoffBankCard2CardFTM RUB -> paymentGuideCard2Card transferMethod transferAddressee cents currency code
                FiatTM QiwiFTM RUB -> paymentGuideQiwi transferMethod transferAddressee cents currency code
                CryptoTM PZM -> paymentGuideCryptoC transferMethod transferAddressee cents currency code
                CryptoTM OURO -> paymentGuideCryptoC transferMethod transferAddressee cents currency code
                _ -> [whamlet|Приём средств временно приостановлен.  Попробуйте позже|]
        defaultLayout $(widgetFile "client/deposit-proceed")
  where
    guideTemplate :: Text -> Int -> Currency -> Text -> Html -> Text -> Widget
    guideTemplate title cents curr desc addr _ = $(widgetFile "client/deposit-guide")
    paymentGuideCard2Card :: TransferMethod -> [Text] -> Int -> Currency -> Text -> Widget
    paymentGuideCard2Card tm addressee cents curr _ =
        guideTemplate (paymentTitle tm) cents curr "на карту" (paymentAddr tm addressee) code
    paymentGuideQiwi tm addressee cents curr _ =
        guideTemplate (paymentTitle tm) cents curr "на Qiwi-кошелёк" (paymentAddr tm addressee) code
    paymentGuideCryptoC tm addressee cents curr _ =
        let c = case tm of
                CryptoTM c' -> c'
                _           -> error "Ошибка.  Неверные данные"
        in guideTemplate (paymentTitle tm) cents curr (cryptoCDesc c) (paymentAddr tm addressee) code
    paymentTitle :: TransferMethod -> Text
    paymentTitle (FiatTM SberBankCard2CardFTM _) = "Пополнение переводом на карту СберБанка"
    paymentTitle (FiatTM TinkoffBankCard2CardFTM _) = "Пополнение переводом на карту Тинькофф Банка"
    paymentTitle (FiatTM QiwiFTM _) = "Перевод на Qiwi кошелёк по номеру телефона"
    paymentTitle (FiatTM _ _) = "Перевод на данный момент не поддерживается"
    paymentTitle (CryptoTM curr) = "Перевод на " <> cCurrencyTLong curr <> " кошелёк"
    paymentAddr :: TransferMethod -> [Text] -> Html
    paymentAddr (FiatTM AlphaBankCard2CardFTM _) _ = [shamlet|Нет адреса для перевода|]
    paymentAddr (FiatTM _ _) adr = [shamlet|#{concat adr}|]
    paymentAddr (CryptoTM PZM) adr =
        let (w, _) = case adr of
                w':pk':_ -> (w', pk')
                w':_     -> (w', "")
                _        -> ("", "")
        in [shamlet|#{w}|]
    paymentAddr (CryptoTM _) adr = [shamlet|#{concat adr}|]
    cryptoCDesc curr = "на " <> cCurrencyTLong curr <> " кошелёк"


postDepositConfirmRequestR :: Handler Html
postDepositConfirmRequestR = do
    clientId <- requireClientId
    -- code <- runInputPost $ ireq textField "transaction-code"
    trid <- fmap toSqlKey $ runInputPost $ ireq intField "transaction-id"
    payer <- runInputPost $ ireq textField "payer-address"
    ch <- appChannelsOperatorDepositConfirm . appChannels <$> getYesod
    withClientRequest trid $ \request@(Entity tid t) ->
        if clientId == depositRequestUserId t
            then do
                let payerRecord = DepositPayer payer tid
                runDB $ do
                    update tid [DepositRequestStatus =. ClientConfirmed]
                    insert payerRecord
                liftIO . atomically $ writeTChan ch request
                setMessageI MsgDepositRequestConfirmedMessage
                redirect DepositR
            else notFound

postClientCancelDepositR :: Handler Html
postClientCancelDepositR = do
    requestId <- fmap toSqlKey $ runInputPost $ ireq intField "request-id"
    withClientRequest requestId $ \(Entity tid _) -> do
        t <- liftIO getCurrentTime
        runDB $ update tid [DepositRequestStatus =. ClientCancelled t]
        setMessageI MsgDepositCancelled
        redirect DepositR

withClientRequestByCode
    :: Text
    -> (Entity DepositRequest -> Handler Html)
    -> Handler Html
withClientRequestByCode code action = do
    clientId <- requireClientId
    r <- runDB . getBy404 $ UniqueDepositRequest code
    if depositRequestUserId (entityVal r) /= clientId
        then notFound
        else action r

withClientRequest
    :: DepositRequestId
    -> (Entity DepositRequest -> Handler Html)
    -> Handler Html
withClientRequest rid action = do
    clientId <- requireClientId
    r <- runDB $ get404 rid
    if depositRequestUserId r /= clientId
        then notFound
        else action (Entity rid r)

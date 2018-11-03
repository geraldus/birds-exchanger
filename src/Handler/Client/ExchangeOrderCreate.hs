{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Client.ExchangeOrderCreate where


import Import
import Local.Persist.Currency
import Local.Persist.Deposit
import Local.Persist.Wallet
import Local.Persist.ExchangeOrder
import Form.Exchanger.Order


postExchangeOrderCreateR :: Handler Html
postExchangeOrderCreateR = do
    clientData <- requireClient
    ((res, widget), enctype) <- runFormPost formCreateExchageOrder
    case res of
        FormMissing -> invalidArgs ["Данные не получены"]
        FormFailure es -> do
            setMessage [shamlet|
                <p .h5>Ошибка создания ордера на обмен
                <ul>
                    $forall e <- es
                        <li>#{e}
                |]
            redirect HomeR
        FormSuccess od@ExchangeOrderData{..} -> do
            let (Entity userId _, wallets) = clientData
            outWallet <- getMatchingWallet userId orderDataCurrencyOut wallets
            if (userWalletAmountCents $ entityVal outWallet) < orderDataAmountCents
                then defaultLayout $ do
                    setTitle [shamlet|
                        <p>Для создания ордера недостаточно средств на счёте
                        <p>Пополните баланс|]
                    redirect HomeR
                else do
                    time <- liftIO getCurrentTime
                    $(logInfo) $ pack $ show od
                    runDB $ do
                        let userWalletId = entityKey outWallet
                        wtrId <- insert $ WalletTransactionReason userWalletId
                        wbtId <- insert $ WalletBalanceTransaction
                            userWalletId
                            (ExchangeFreeze orderDataAmountCents)
                            wtrId
                            (userWalletAmountCents . entityVal $ outWallet)
                        update userWalletId [UserWalletAmountCents -=. orderDataAmountCents]
                        insert $ ExchangeOrder
                            userId
                            orderDataCurrencyOut
                            orderDataCurrencyIn
                            orderDataAmountCents
                            orderDataRatio
                            orderDataExpectedFeeCents
                            time
                            (Created time)
                            wtrId
                    defaultLayout $ do
                        setMessage [shamlet|<p .h5>Ордер успешно создан|]
                        redirect HomeR



getMatchingWallet :: UserId -> Currency -> [Entity UserWallet] -> Handler (Entity UserWallet)
getMatchingWallet userId currency wallets = do
    let mwallet = find (\(Entity _ UserWallet{..}) -> userWalletCurrency == currency) wallets
    case mwallet of
        Nothing -> getOrCreateWallet userId currency
        Just wallet -> return wallet


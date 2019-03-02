{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Home where

import           Import                       hiding ( decodeUtf8, httpLbs )

import           Form.Exchanger.Order
import           Local.Persist.Currency
import           Local.Persist.Exchange       ( ExchangePair (..) )
import           Utils.Deposit
import           Utils.Money
import           Utils.Render
import           Utils.Withdrawal

import           Data.Text.Lazy.Encoding      ( decodeUtf8 )
import           Network.HTTP.Client.Internal
import           Network.HTTP.Client.TLS
import           Text.Julius                  ( RawJS (..) )


-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo        :: FileInfo
    , fileDescription :: Text
    }

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    wrapId <- newIdent
    ratioId <- newIdent
    modalWrapId <- newIdent
    modalRatioId <- newIdent
    (mmsg, mayClientUser, orderCreateFormW, modalOrderCreateFormW, (pzmRurOrders, rurPzmOrders)) <- getData wrapId modalWrapId ratioId modalRatioId
    (btcARes, cbrRes) <- runResourceT $ liftIO $ do
        manager <- newTlsManager
        btcAReq <- parseRequest "https://btc-alpha.com/exchange/PZM_USD"
        cbrReq <- parseRequest "http://cbr.ru"
        btcARes <- httpLbs btcAReq manager
        cbrRes <- httpLbs cbrReq manager
        return (rbt btcARes, rbt cbrRes)
    defaultLayout $ do
        setTitleI MsgProjectName
        $(widgetFile "homepage")
  where
    rbt = decodeUtf8 . responseBody

getData :: Text -> Text -> Text -> Text -> HandlerFor App (Maybe Html, Maybe (Key User), Widget, Widget,
        ([Entity ExchangeOrder], [Entity ExchangeOrder]))
getData wrapId modalWrapId ratioId modalRatioId = do
    mUser <- maybeClientUser
    (,,,,)
        <$> getMessage
        <*> pure mUser
        <*> fmap fst (generateFormPost (createOrderForm wrapId ratioId ExchangePzmRur))
        <*> fmap fst (generateFormPost (createOrderForm modalWrapId modalRatioId ExchangePzmRur))
        <*> getActiveOrders mUser

maybeClientUser :: HandlerFor App (Maybe (Key User))
maybeClientUser = (entityKey . fst <$>) <$> maybeClient


getActiveOrders :: Maybe UserId -> Handler ([Entity ExchangeOrder], [Entity ExchangeOrder])
getActiveOrders mu = do
    $(logInfo) $ pack $ show mu
    let userConstraint = case mu of
            Nothing -> []
            Just _  -> [] -- [ExchangeOrderUserId !=. uid]
    os <- runDB $ selectList
        ((ExchangeOrderIsActive ==. True) : userConstraint)
        [Asc ExchangeOrderNormalizedRatio, Asc ExchangeOrderCreated]
    return $ partition (isPzmRurOrder . entityVal) os
    where isPzmRurOrder = (== ExchangePzmRur) . exchangeOrderPair


renderOrderCol :: Text -> Text -> [ExchangeOrder] -> Widget
renderOrderCol pair title orders =
    [whamlet|
        <h5 .text-center>#{title}
        <table .table .table-hover data-pair="#{pair}">
            <thead .thead-dark>
                <tr>
                    <th>Ставка
                    <th>Кол-во
                    <th>Сумма
            <tbody>
                $forall order <- orders
                    <tr .clickable-order>
                        <td .ratio>
                            #{show (exchangeOrderNormalizedRatio order)}
                        <td .amount-left>
                            #{cents2dblT (exchangeOrderAmountLeft order)}
                        <td .expected>
                            #{cents2dblT (convertCents (normalizeRatio (exchangeOrderPair order) (exchangeOrderRatioNormalization order) (exchangeOrderNormalizedRatio order)) (exchangeOrderAmountLeft order))}
        |]


clickableOrderW :: Text -> Widget
clickableOrderW wrapId = toWidget [julius|
    const handleOrderClick = (e) => {
        const row = $(e.currentTarget)
        const table = row.parents('table').first()
        const form = $('##{rawJS wrapId}')
        const ratioInput = $('.ratio-input', form)
        const amountInput = $('.amount-input', form)
        const actionInput = $('.exchange-action-input', form)
        const actions = $('option', actionInput)
        const ratio = $('.ratio', row).text()
        const amountLeft = $('.amount-left', row).text()
        const expected = $('.expected', row).text()
        const action = table.data('pair')
        ratioInput.val(ratio)
        actions.removeAttr('selected')
        switch (action) {
            case 'rur_pzm':
                $(actions[0]).attr('selected', 'selected')
                amountInput.val(expected)
                break
            case 'pzm_rur':
            default:
                amountInput.val(amountLeft)
                $(actions[1]).attr('selected', 'selected')
                break
        }
        amountInput.change()
        $('#clickable-order-modal').modal('show')
    }
    $(document).ready(() => {
        const orders = $('.clickable-order')
        orders.click(handleOrderClick)
        // fix input sizes within modal
        const divs = $('#clickable-order-modal .form-group.row').first().find('div')
        divs.each((i, el) => {
            el.className = el.className.replace(/col-\d/g, '')
            el.className = el.className.replace(/col-lg-\d/g, '')
            el.className = el.className.replace(/  /g, ' ')
        })
        const form = $('##{rawJS wrapId}')
        const actionInput = $('.exchange-action-input', form)
        actionInput.attr('readonly', 'readonly')
    })
    |]

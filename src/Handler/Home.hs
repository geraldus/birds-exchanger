{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Home where

import           Import                  hiding ( decodeUtf8, httpLbs )

import           Form.Exchanger.Order
import           Handler.API.Order.Index
import           Local.Params
import           Local.Persist.Currency
import           Local.Persist.Exchange  ( ExchangePair (..) )
import           Utils.Money
import           Utils.Render

import qualified Data.HashMap.Strict     as HMS
import           Data.Text.Lazy.Encoding ( decodeUtf8 )
import           Prelude                 ( foldl )
import           Text.Julius             ( RawJS (..) )


-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo        :: FileInfo
    , fileDescription :: Text
    }

getHomeR :: Handler Html
getHomeR = do
    wrapId <- newIdent
    ratioId <- newIdent
    modalWrapId <- newIdent
    modalRatioId <- newIdent
    (mmsg, mayClientUser, orderCreateFormW, modalOrderCreateFormW) <-
            getData wrapId modalWrapId ratioId modalRatioId
    -- Sort RUR to PZM orders by descending order ratio
    let featured = featuredModal
    orders <- runDB $ flip mapM [ ExchangePzmRur, ExchangeOurRur, ExchangeOurPzm ] selectActiveOrdersOf
    let statsDOM = reduceDomStats [] $ concat orders
    renderMessage <- getMessageRender
    defaultLayout $ do
        setAppPageTitle MsgHomePageTitle
        $(widgetFile "homepage")
  where
    rbt = decodeUtf8 . responseBody

getData :: Text -> Text -> Text -> Text -> HandlerFor App (Maybe Html, Maybe (Key User), Widget, Widget)
getData wrapId modalWrapId ratioId modalRatioId = do
    mUser <- maybeClientUser
    (,,,)
        <$> getMessage
        <*> pure mUser
        <*> fmap fst (generateFormPost
                (createOrderForm wrapId ratioId ExchangePzmRur))
        <*> fmap fst (generateFormPost
                (createOrderForm modalWrapId modalRatioId ExchangePzmRur))

maybeClientUser :: HandlerFor App (Maybe (Key User))
maybeClientUser = (entityKey . fst <$>) <$> maybeClient


getActiveOrders :: Maybe UserId -> Handler ([Entity ExchangeOrder], [Entity ExchangeOrder])
getActiveOrders mu = do
    let userConstraint = case mu of
            Nothing -> []
            Just _  -> [] -- [ExchangeOrderUserId !=. uid]
    os <- runDB $ selectList
        ((ExchangeOrderIsActive ==. True) : userConstraint)
        [Asc ExchangeOrderNormalizedRatio, Asc ExchangeOrderCreated]
    return $ partition (isPzmRurOrder . entityVal) os
    where isPzmRurOrder = (== ExchangePzmRur) . exchangeOrderPair

renderOrderLeftCol :: ExchangePair -> [ExchangeOrder] -> Widget
renderOrderLeftCol exchange orders =
    renderOrderTable exchange True False widget
    where widget
            | null orders = noOrdersColContent
            | otherwise = foldr (\o w -> w >> renderOrderRow o) mempty orders

renderOrderRightCol :: ExchangePair -> [ExchangeOrder] -> Widget
renderOrderRightCol exchange orders =
    renderOrderTable exchange False False widget
    where widget
            | null orders = noOrdersColContent
            | otherwise = foldr (\o w -> w >> renderOrderRow o) mempty orders

renderOrderTable :: ExchangePair -> Bool -> Bool -> Widget -> Widget
renderOrderTable exchange flip' hidden tableBodyWidget =
    [whamlet|
        <h5 :hidden:.hide .text-center data-pair="#{epair}">#{title}
        <table :hidden:.hide .table .table-hover data-pair="#{epair}">
            <thead .thead-dark>
                <tr>
                    <th>_{MsgRatio}
                    <th>_{MsgQuantityShort} #
                        <span .text-muted>(#{currSign c1})
                    <th>_{MsgAmount} #
                        <span .text-muted>(#{currSign c2})
            <tbody>
                ^{tableBodyWidget}
        |]
  where
    (c1, c2) = (\(a, b) -> if flip' then (b, a) else (a, b)) $
        unPairCurrency exchange
    c1code = toLower $ currencyCodeT c1
    c2code = toLower $ currencyCodeT c2
    c1sign = currSign c1
    c2sign = currSign c2
    epair  = c1code <> "_" <> c2code
    title  = c1sign <> " ⇢ " <> c2sign


renderOrderRow :: ExchangeOrder -> Widget
renderOrderRow order =
    let pair'         = exchangeOrderPair order
        nRatio        = exchangeOrderNormalizedRatio order
        normalization = exchangeOrderRatioNormalization order
        amtLeft       = exchangeOrderAmountLeft order
        cents         = multiplyCents
                (normalizeRatio pair' normalization nRatio) amtLeft
    in [whamlet|
        <tr .clickable-order>
            <td .ratio>
                #{show (nRatio)}
            <td .amount-left>
                #{cents2dblT (amtLeft)}
            <td .expected>
                #{cents2dblT (cents)}
        |]

noOrdersColContent :: Widget
noOrdersColContent = [whamlet|
    <tr rowspan="2">
        <td colspan="3" .text-center .align-middle>
            ОРДЕРОВ НА ОБМЕН ЕЩЁ НЕТ
            <br />
            <small>
                Станьте первым, кто создаст новый ордер
    |]


comingSoonColContent :: Widget
comingSoonColContent = [whamlet|
    <tr rowspan="2">
        <td colspan="3" .text-center .align-middle>
            СКОРО
            <br />
            <small>
                Следите за новостями
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
            case 'pzm_our':
            case 'rur_our':
                $(actions[0]).attr('selected', 'selected')
                form.removeClass('action-give').addClass('action-take')
                amountInput.val(expected)
                break
            case 'pzm_rur':
            default:
                amountInput.val(amountLeft)
                form.removeClass('action-take').addClass('action-give')
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

featuredModal :: Widget
featuredModal = do
    mayFeatured <- handlerToWidget getLastFeaturedNews
    case mayFeatured of
        Nothing -> [whamlet||]
        Just (Entity iid info)  -> do
            let desc = case infoDescHtml info of
                    Just ""    -> infoContentHtml info
                    Just desc' -> desc'
                    _          -> infoContentHtml info
            [whamlet|
                <div #featured-modal .modal .fade tabindex="-1" role="dialog">
                    <div .modal-dialog .modal-dialog-centered role="document">
                        <div .modal-content .text-white style="background-color: #0e0e0e">
                            <div .container-fluid>
                                $maybe thumb <- infoThumbUrl info
                                    <div .row>
                                        <div .col-12>
                                            <img
                                                style="max-width: 100%"
                                                src="#{thumb}"
                                                alt="Иконка новости"/>
                                <div .row>
                                    <div .col-10 .mx-auto .py-3>
                                        #{preEscapedToMarkup desc}
                                        <span style="float: right">
                                            <a href="@{InfoViewR (infoAlias info)}">Подробнее...
                                        |]
            toWidget [julius|$('#featured-modal').modal('show');
                |]


getLastFeaturedNews :: Handler (Maybe (Entity Info))
getLastFeaturedNews = do
    allNews <- runDB $ selectList [ InfoFeatured ==. True ] [ Desc InfoCreated, LimitTo 1 ]
    case allNews of
        []  -> return Nothing
        x:_ -> return (Just x)

renderDomTable :: ExchangePair -> Bool -> Bool -> DomStats -> Widget
renderDomTable p buy hidden d = domTable pair hidden title body
    where
        pair = if buy then flipPair p else p
        pairStats = (sortBy (flip (comparing fst)) . HMS.toList) <$> (HMS.lookup pair d)
        maxCount = fromMaybe 0 $ (foldr max 0 . map ((\(a, _, _) -> a) . snd)) <$> pairStats
        title = if buy
            then currSign inc <> " ⇢ " <> currSign outc <> " BID"
            else currSign outc <> " ⇢ " <> currSign inc <> " ASK"
        body = (concatMap $ \(r, s) -> domRow r maxCount buy s) <$> pairStats
        (outc, inc) = unPairCurrency p

domRow :: Double -> Int -> Bool -> Dom -> Widget
domRow r t buy d =
    let (count, outCents, inCents) = d
        leftd = "left" :: Text
        rightd = "right" :: Text
        (direction, color) = if buy
            then (leftd, "#47b9002b")
            else (rightd, "#ff23002b")
        widtht = round $ (fromIntegral count) / (fromIntegral t) * 100
        widthf = 100 - widtht
        style = concat
            [ "background: linear-gradient(to "
            , direction <> ", "
            , "#fff0 " <> (pack . show $ widthf) <> "%, "
            , color <> " " <> (pack . show $ widthf) <> "%);" ]
    in [whamlet|
        <tr
            .clickable-order
            style=#{style}
        >
            <td .ratio>
                #{cents2dblT (round r * 100)}
            <td .amount-left>
                #{cents2dblT outCents}
            <td .expected>
                #{cents2dblT inCents}
            <td .depth>
                #{show count}
        |]

domTable :: ExchangePair -> Bool -> Text -> Maybe Widget -> Widget
domTable pair hidden title mbody  =
    let (outc, inc) = unPairCurrency pair
        expair = intercalate "_" . map (toLower . currencyCodeT) $ [outc, inc]
        body = fromMaybe (emptyList 10 4) mbody
    in
        [whamlet|
            <h5 :hidden:.hide .text-center data-pair="#{expair}">#{title}
            <table :hidden:.hide .table .table-hover data-pair="#{expair}">
                <thead .thead-dark>
                    <tr>
                        <th>_{MsgRatio}
                        <th>_{MsgAmount} #
                            <span .text-muted>(#{currSign outc})
                        <th>_{MsgAmount} #
                            <span .text-muted>(#{currSign inc})
                        <th>_{MsgQuantityShort}
                <tbody>
                    ^{body}
        |]

emptyList :: Int -> Int -> Widget
emptyList row col = [whamlet|
    <tr rowspan=#{row}>
        <td colspan=#{col} .text-uppercase .text-center .align-middle>
            _{MsgDomNoOrders}
            <br />
            <small>
                _{MsgDomCreateNewOrderOffer}
        |]

genericDomRender
    :: ExchangePair
    -> DomStats
    -> (ExchangePair -> ExchangePair)
    -> (ExchangePair -> Text)
    -> (Text -> Maybe Widget -> Widget)
    -> (ExchangePairDom -> Widget)
    -> Widget
genericDomRender p ds direction title wrapper itemRender =
        wrapper (title p) $ itemRender <$> HMS.lookup (direction p) ds

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Home where

import           Import                 hiding ( decodeUtf8, httpLbs, on )

import           Form.Exchanger.Order
import           Local.Params
import           Local.Persist.Currency
import           Local.Persist.Exchange ( ExchangePair (..) )
import           Market.Functions       ( reduceDomStats )
import           Market.Type            ( DOMRateStats, DOMStats,
                                          DOMStatsRateMap )
import           Type.Money             ( oneCoinCents )
import           Utils.Common           ( selectLocale )
import           Utils.Database.Orders  ( selectActiveOrdersOf )
import           Utils.Money
import           Utils.Render
import           Utils.Time             ( renderDate, renderTime,
                                          timezoneOffsetFromCookie )

import qualified Data.HashMap.Strict    as HMS
import           Database.Esqueleto     ( InnerJoin (..), desc, from, in_,
                                          limit, on, orderBy, valList, where_,
                                          (^.) )
import qualified Database.Esqueleto     as E
import           Database.Persist.Sql   ( fromSqlKey )
import           Text.Julius            ( RawJS (..) )


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
    paramsFrom <- lookupGetParam "from"
    paramsTo <- lookupGetParam "to"
    let paramsPair = selectPair paramsFrom paramsTo
    (messages, mayClientUser, orderCreateFormW, modalOrderCreateFormW) <-
            getData wrapId modalWrapId ratioId modalRatioId (flipPair paramsPair)
            -- flipping paramsPair gives right tab (and form) exchange direction
            -- (defPairDir seems to always be opposite form pair in current form
            -- implementation)
    let featured = featuredModal
    let defaultPairs = [ ExchangePzmRur, ExchangeOurRur, ExchangeOurPzm ]
    orders <- runDB $ mapM selectActiveOrdersOf defaultPairs
    exchanges <- mapM getLastExchangesOf defaultPairs
    let exHistory = exchangeHistoryW (zip defaultPairs exchanges)
    let statsDOM = reduceDomStats [] $ concat orders
    messageRender <- getMessageRender
    defaultLayout $ do
        setAppPageTitle MsgHomePageTitle
        $(widgetFile "homepage")
  where
    selectPair Nothing Nothing = defPairDir ExchangePzmRur
    selectPair _ Nothing       = defPairDir ExchangePzmRur
    selectPair Nothing _       = defPairDir ExchangePzmRur
    selectPair (Just c1) (Just c2)
        | (c1 == "rur" || c1 == "rub") && c2 == "our" =
            defPairDir ExchangeRurOur
        | (c2 == "rur" || c2 == "rub") && c1 == "our" =
            defPairDir ExchangeOurRur
        | c1 == "pzm" && c2 == "our" =
            defPairDir ExchangePzmOur
        | c2 == "pzm" && c1 == "our" =
            defPairDir ExchangeOurPzm
        | otherwise = defPairDir ExchangePzmRur


-- | == Utils

getData ::
       Text
    -> Text
    -> Text
    -> Text
    -> ExchangePair
    -> HandlerFor App ([(Text, Html)], Maybe (Key User), Widget, Widget)
getData wrapId modalWrapId ratioId modalRatioId exdir = do
    mUser <- maybeClientUser
    (,,,)
        <$> getMessages
        <*> pure mUser
        <*> fmap fst (generateFormPost
                (createOrderForm wrapId ratioId exdir))
        <*> fmap fst (generateFormPost
                (createOrderForm modalWrapId modalRatioId exdir))

maybeClientUser :: HandlerFor App (Maybe (Key User))
maybeClientUser = (entityKey . fst <$>) <$> maybeClient

getActiveOrders :: Maybe UserId -> Handler ([Entity ExchangeOrder], [Entity ExchangeOrder])
getActiveOrders mu = do
    let userConstraint = case mu of
            Nothing -> []
            Just _  -> []
    os <- runDB $ selectList
        ((ExchangeOrderIsActive ==. True) : userConstraint)
        [Asc ExchangeOrderNormalizedRatio, Asc ExchangeOrderCreated]
    return $ partition (isPzmRurOrder . entityVal) os
    where isPzmRurOrder = (== ExchangePzmRur) . exchangeOrderPair

clickableOrderW :: Text -> Widget
clickableOrderW wrapId = toWidget [julius|
    const handleOrderClick = (e) => {
        const row = $(e.currentTarget)
        const parent = row.parents('.dom-view').first()
        const form = $('##{rawJS wrapId}')
        const ratioInput = $('.ratio-input', form)
        const amountInput = $('.amount-input', form)
        const actionInput = $('.exchange-action-input', form)
        const actions = $('option', actionInput)
        const ratio = $('.ratio', row).text()
        const amountLeft = $('.amount-left', row).text()
        const expected = $('.expected', row).text()
        const action = parent.data('pair')
        ratioInput.val(ratio)
        actions.removeAttr('selected')
        switch (action) {
            case 'rub_pzm':
            case 'pzm_our':
            case 'rub_our':
                $(actions[0]).attr('selected', 'selected')
                form.removeClass('action-give').addClass('action-take')
                amountInput.val(expected)
                break
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
            wrapId <- newIdent
            [whamlet|
                <div
                    #featured-modal
                    .modal
                    .fade
                    tabindex="-1"
                    role="dialog"
                    data-newsid="#{fromSqlKey iid}"
                    >
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
                                        <div style="float: left; cursor: pointer; user-select: none">
                                            <span .checkmark>✓
                                            <span ##{wrapId}-remember-trigger .text-muted>
                                                _{MsgDoNotShowAgain}
                                        <div style="float: right">
                                            <a href="@{InfoViewR (infoAlias info)}">
                                                _{MsgReadMore}
                                        |]
            toWidget [julius|
            $(document).ready(() => {
                const featured = '#{rawJS (show $ fromSqlKey iid)}'
                const cookieName = 'outb.info_featured'
                const newsCookie = Cookies.get(cookieName)
                const markedNews =
                    newsCookie && new Set(JSON.parse(newsCookie)) || new Set()
                const trigger = $('##{rawJS wrapId}-remember-trigger')
                const toggleFeaturedVisibility = () => {
                    if (trigger.parent().hasClass('toggle')) {
                        markedNews.delete(featured)
                    } else {
                        markedNews.add(featured)
                    }
                    Cookies.set(cookieName, JSON.stringify([ ...markedNews ]), { domain: 'outb.info', expires: 60 })
                    Cookies.set(cookieName, JSON.stringify([ ...markedNews ]), { domain: 'localhost', expires: 60 })
                    trigger.parent().toggleClass('toggle')
                }
                if (!newsCookie || !markedNews.has(featured)) {
                    $('#featured-modal').modal('show')
                }
                trigger.click(toggleFeaturedVisibility)
            });
            |]

getLastFeaturedNews :: Handler (Maybe (Entity Info))
getLastFeaturedNews = do
    allNews <- runDB $ selectList [ InfoFeatured ==. True ] [ Desc InfoCreated, LimitTo 1 ]
    case allNews of
        []  -> return Nothing
        x:_ -> return (Just x)

-- | ** Depth of Market

renderDomTable :: ExchangePair -> Bool -> Bool -> DOMStats -> Widget
renderDomTable p buy hidden d = domDivView pair' hidden title body
    where
        pair' = if buy then flipPair p else p
        pairStats =
            sortBy (flip (comparing fst)) . HMS.toList <$> HMS.lookup pair' d
        maxCount =
            maybe 0 (foldr max 0 . map ((\(_, b, _) -> b) . snd)) pairStats
        title = if buy
            then [shamlet|#{currSign inc} ⇢ #{currSign outc}|]
            else [shamlet|#{currSign outc} ⇢ #{currSign inc}|]
        body = (concatMap $ \(r, s) -> domDivRow r maxCount buy s) <$> pairStats
        (outc, inc) = unPairCurrency p

domRow :: Double -> Int -> Bool -> DOMRateStats -> Widget
domRow r t buy d =
    let (ordersCount, outCents, inCents) = d
        leftd = "left" :: Text
        rightd = "right" :: Text
        (direction, color) = if buy
            then (leftd, "#47b9002b")
            else (rightd, "#ff23002b")
        barWidth :: Double
        barWidth = fromIntegral (outCents * oneCoinCents) / fromIntegral t
        widtht :: Int
        widtht = round barWidth
        widthf = 100 - widtht
        style = concat
            [ "background: linear-gradient(to "
            , direction <> ", "
            , "#fff0 " <> (pack . show $ widthf) <> "%, "
            , color <> " " <> (pack . show $ widthf) <> "%);" ]
    in $(widgetFile "dom/table/row")

domTable :: ExchangePair -> Bool -> Html -> Maybe Widget -> Widget
domTable pair' hidden title mbody  =
    let (outc, inc) = unPairCurrency pair'
        expair = intercalate "_" . map (toLower . currencyCodeT) $ [outc, inc]
        body = fromMaybe (emptyList 10 4) mbody
    in $(widgetFile "dom/table/table")

domDivRow :: Double -> Int -> Bool -> DOMRateStats -> Widget
domDivRow r t buy d =
    let (ordersCount, outCents, inCents) = d
        leftd = "left" :: Text
        rightd = "right" :: Text
        (direction, color) = if buy
            then (leftd, "#47b9002b")
            else (rightd, "#ff23002b")
        barWidth :: Double
        barWidth = fromIntegral (outCents * oneCoinCents) / fromIntegral t
        widtht :: Int
        widtht = round barWidth
        widthf = 100 - widtht
        style = concat
            [ "background: linear-gradient(to "
            , direction <> ", "
            , "#fff0 " <> (pack . show $ widthf) <> "%, "
            , color <> " " <> (pack . show $ widthf) <> "%);" ]
    in $(widgetFile "dom/divs/row")

domDivView :: ExchangePair -> Bool -> Html -> Maybe Widget -> Widget
domDivView pair' hidden title mbody  =
    let (outc, inc) = unPairCurrency pair'
        expair = intercalate "_" . map (toLower . currencyCodeT) $ [outc, inc]
        body = fromMaybe (emptyList 10 4) mbody
    in $(widgetFile "dom/divs/view")

emptyList :: Int -> Int -> Widget
emptyList row col = [whamlet|
    <tr rowspan=#{row}>
        <td colspan=#{col} .text-uppercase .text-center .align-middle>
            _{MsgDomNoOrders}
            <br />
            <small>
                _{MsgDomCreateNewOrderOffer}
        |]

genericDomRender ::
       ExchangePair
    -> DOMStats
    -> (ExchangePair -> ExchangePair)
    -> (ExchangePair -> Text)
    -> (Text -> Maybe Widget -> Widget)
    -> (DOMStatsRateMap -> Widget)
    -> Widget
genericDomRender p ds direction title wrapper itemRender =
        wrapper (title p) $ itemRender <$> HMS.lookup (direction p) ds


getLastExchangesOf ::
       ExchangePair
    -> Handler [(Entity ExchangeOrderExecution, Entity ExchangeOrder)]
getLastExchangesOf p = runDB . E.select . from $ \(o `InnerJoin` e) -> do
    on (o ^. ExchangeOrderId E.==. e ^. ExchangeOrderExecutionOrderId)
    where_ (o ^. ExchangeOrderPair `in_` valList [ p, flipPair p ])
    orderBy [desc (e ^. ExchangeOrderExecutionTime)]
    limit 10
    return (e, o)
  where
    (cOut, cIn) = unPairCurrency p

exchangeHistoryW ::
       [(ExchangePair, [(Entity ExchangeOrderExecution, Entity ExchangeOrder)])]
    -> ExchangePair
    -> Widget
exchangeHistoryW groups activePair = do
    mapM_ lastExchangesW' groups
  where
    lastExchangesW' (p, exs) =
        let (cOut, cIn) = unPairCurrency p
            isVisible = p == activePair
        in lastExchangesW cOut cIn exs isVisible

lastExchangesW ::
       Currency
    -> Currency
    -> [(Entity ExchangeOrderExecution, Entity ExchangeOrder)]
    -> Bool
    -> Widget
lastExchangesW cOut cIn exs visible = [whamlet|
    <div
        .exchange-history
        .container-fluid
        data-from=#{outCurrencyCode}
        data-to=#{inCurrencyCode}
        :visible:.show
        :not visible:.hide
        >
        <h5 .text-center>
            _{MsgExchangePairHistory (toUpper outCurrencyCode) (toUpper inCurrencyCode)}
        <div .header .row>
            <div .col-2>_{MsgExchangeTime}
            <div .col-2>_{MsgExchangeType}
            <div .col-2>_{MsgExchangeRate}
            <div .col-3>#{toUpper outCurrencyCode}
            <div .col-3>#{toUpper inCurrencyCode}
        ^{body}
    |]
  where
    outCurrencyCode = currencyCodeT' cOut
    inCurrencyCode  = currencyCodeT' cIn
    body = mapM_ exchangeW exs
    exchangeW (Entity _ ex, Entity _ o)= do
        (render, l, tz) <- handlerToWidget $ (,,)
            <$> getAmountRenderer
            <*> selectLocale
            <*> timezoneOffsetFromCookie
        let outAmount = render [] [] False cOut outCents
            inAmount  = render [] [] False cIn  inCents
            time = renderTime l tz exTime
            date = renderDate l tz exTime
        [whamlet|<div .row .exchange>
            <div .col-2 title="#{date}">
                <small>
                    #{time}
            <div .col-2 .text-lowercase>
                ^{typW}
            <div .col-2>
                #{rate}
            <div .col-3>
                #{outAmount}
            <div .col-3>
                #{inAmount}
            |]
      where
        exTime = exchangeOrderExecutionTime ex
        orderPair = exchangeOrderPair o
        isAsk = unPairCurrency orderPair == (cOut, cIn)
        transfer = exchangeOrderExecutionTransferAmountCents ex
        income = exchangeOrderExecutionIncomeAmountCents ex
        rate = fixedDoubleT 2 (exchangeOrderNormalizedRatio o)
        (outCents, inCents) = if isAsk
            then (transfer, income)
            else (income, transfer)
        typW = if isAsk
                then [whamlet|<span .ask>_{MsgExchangeAsk}|]
                else [whamlet|<span .bid>_{MsgExchangeBid}|]

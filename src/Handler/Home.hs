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
import           Database.Persist.Sql    ( fromSqlKey )
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
    paramsFrom <- lookupGetParam "from"
    paramsTo <- lookupGetParam "to"
    let paramsPair = selectPair paramsFrom paramsTo
    (mmsg, mayClientUser, orderCreateFormW, modalOrderCreateFormW) <-
            getData wrapId modalWrapId ratioId modalRatioId (flipPair paramsPair)
            -- flipping paramsPair gives right tab (and form) exchange direction
            -- (defPairDir seems to always be opposite form pair in current form
            -- implementation)
    let featured = featuredModal
    orders <- runDB $ flip mapM [ ExchangePzmRur, ExchangeOurRur, ExchangeOurPzm ] selectActiveOrdersOf
    let statsDOM = reduceDomStats [] $ concat orders
    renderMessage <- getMessageRender
    defaultLayout $ do
        setAppPageTitle MsgHomePageTitle
        $(widgetFile "homepage")
  where
    rbt = decodeUtf8 . responseBody
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


getData :: Text -> Text -> Text -> Text -> ExchangePair -> HandlerFor App (Maybe Html, Maybe (Key User), Widget, Widget)
getData wrapId modalWrapId ratioId modalRatioId exdir = do
    mUser <- maybeClientUser
    (,,,)
        <$> getMessage
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
            Just _  -> [] -- [ExchangeOrderUserId !=. uid]
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

renderDomTable :: ExchangePair -> Bool -> Bool -> DomStats -> Widget
renderDomTable p buy hidden d = domDivView pair hidden title body
    where
        pair = if buy then flipPair p else p
        pairStats = (sortBy (flip (comparing fst)) . HMS.toList) <$> (HMS.lookup pair d)
        maxCount = maybe 0 (foldr max 0 . map ((\(_, b, _) -> b) . snd)) pairStats
        title = if buy
            then [shamlet|
                    \#{currSign inc} ⇢ #{currSign outc} #
                    <small .text-warning>BID
                    |]
            else [shamlet|
                    \#{currSign outc} ⇢ #{currSign inc} #
                    <small .text-warning>ASK
                    |]
        body = (concatMap $ \(r, s) -> domDivRow r maxCount buy s) <$> pairStats
        (outc, inc) = unPairCurrency p

domRow :: Double -> Int -> Bool -> Dom -> Widget
domRow r t buy d =
    let (count, outCents, inCents) = d
        leftd = "left" :: Text
        rightd = "right" :: Text
        (direction, color) = if buy
            then (leftd, "#47b9002b")
            else (rightd, "#ff23002b")
        widtht = round $ (fromIntegral outCents) / (fromIntegral t) * 100
        widthf = 100 - widtht
        style = concat
            [ "background: linear-gradient(to "
            , direction <> ", "
            , "#fff0 " <> (pack . show $ widthf) <> "%, "
            , color <> " " <> (pack . show $ widthf) <> "%);" ]
    in $(widgetFile "dom/table/row")

domTable :: ExchangePair -> Bool -> Html -> Maybe Widget -> Widget
domTable pair hidden title mbody  =
    let (outc, inc) = unPairCurrency pair
        expair = intercalate "_" . map (toLower . currencyCodeT) $ [outc, inc]
        body = fromMaybe (emptyList 10 4) mbody
    in $(widgetFile "dom/table/table")

domDivRow :: Double -> Int -> Bool -> Dom -> Widget
domDivRow r t buy d =
    let (count, outCents, inCents) = d
        leftd = "left" :: Text
        rightd = "right" :: Text
        (direction, color) = if buy
            then (leftd, "#47b9002b")
            else (rightd, "#ff23002b")
        widtht = round $ (fromIntegral outCents) / (fromIntegral t) * 100
        widthf = 100 - widtht
        style = concat
            [ "background: linear-gradient(to "
            , direction <> ", "
            , "#fff0 " <> (pack . show $ widthf) <> "%, "
            , color <> " " <> (pack . show $ widthf) <> "%);" ]
    in $(widgetFile "dom/divs/row")


domDivView :: ExchangePair -> Bool -> Html -> Maybe Widget -> Widget
domDivView pair hidden title mbody  =
    let (outc, inc) = unPairCurrency pair
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

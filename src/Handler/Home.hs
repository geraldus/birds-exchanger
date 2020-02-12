{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Home where

import           Import                 hiding ( decodeUtf8, httpLbs, on )

import           Form.Exchanger.Order
import           Handler.Stocks         ( findStocksActive )
import           Local.Params
import           Local.Persist.Currency
import           Local.Persist.Exchange ( ExchangePair (..) )
import           Local.Persist.UserRole ( UserRole (Client) )
import           Market.Functions       ( reduceDomStats )
import           Market.Type            ( DOMRateStats, DOMStats,
                                          DOMStatsRateMap )
import           Type.Money             ( oneCoinCents )
import           Utils.Common           ( projectNameHost, selectLocale )
import           Utils.Database.Orders  ( selectActiveOrdersOf )
import           Utils.Database.Stocks  ( queryStocksActives )
import           Utils.Money
import           Utils.Render
import           Utils.Time             ( renderDate, renderTime,
                                          timezoneOffsetFromCookie )

import qualified Data.HashMap.Strict    as HMS
import           Database.Esqueleto     ( InnerJoin (..), asc, desc, from, in_,
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
    projType <- appType . appSettings <$> getYesod
    if projType == FenixApp
        then getFenixTradingHomeR
        else getOutbInfoHomeR

getFenixTradingHomeR :: Handler Html
getFenixTradingHomeR = do
    (messages, _) <- getCommonData
    renderUrl     <- getUrlRender
    stocksActives <- liftHandler . runDB $ queryStocksActives
    auth          <- maybeClientCreds
    let (username, verKey) = case auth of
            Just (Entity _ (User ident _ Client), (Entity _ (Email _ _ key))) -> (ident, key)
            _ -> mempty
        hasVerifiedEmail = isNothing verKey
    let featured = featuredModal
    let bgSrc = StaticR images_bg_050119_png
        whitepaperBgSrc = renderUrl $ StaticR images_wp060119_0002_png
    let stocksAvailabilityW = renderActivesLeft stocksActives
    defaultLayout $ do
        setAppPageTitle MsgHomePageTitle
        $(widgetFile "page/index/generic")
        $(widgetFile "messages/fenix-stocks/pack-desc/generic")
        $(widgetFile "index")
  where
    renderActivesLeft ::
        [(Entity StocksActive, Entity Stocks)] -> Text -> Widget
    renderActivesLeft stocks abr = do
        let (avail, total) = findStocksActive abr stocks
        [whamlet|
            <div .mt-2 .stocks-avail .text-center>
                <span .text-capitalize>
                    _{MsgStocksAmountLeftShort}
                <span .amount-left>
                    #{show avail}
                <span>
                    _{MsgStocksAmountLeftOf}
                <span .amount-total>
                    #{show total}
            |]

getOutbInfoHomeR :: Handler Html
getOutbInfoHomeR = do
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
    let dropEven []             = []
        dropEven (x : [])       = [x]
        dropEven (x : _ : [])   = [x]
        dropEven (x : _ : rest) = x : dropEven rest
    exchanges <- mapM ((dropEven <$>) <$> getLastExchangesOf) defaultPairs
    let exHistory = exchangeHistoryW (zip defaultPairs exchanges)
    let statsDOM = reduceDomStats [] $ concat orders
    messageRender <- getMessageRender
    projType <- appType . appSettings <$> getYesod
    let logoSrc = if projType == FenixApp
            then StaticR images_logo_060119_png
            else StaticR images_logo_outb_info_png
    let bgSrc = StaticR images_bg_050119_png
    defaultLayout $ do
        setAppPageTitle MsgHomePageTitle
        $(widgetFile "page/index/generic")
        $(widgetFile "homepage")

-- * Utils

getData ::
       Text
    -> Text
    -> Text
    -> Text
    -> ExchangePair
    -> HandlerFor App ([(Text, Html)], Maybe (Key User), Widget, Widget)
getData wrapId modalWrapId ratioId modalRatioId exdir = do
    (messages, user) <- getCommonData
    (,,,)
        <$> pure messages
        <*> pure user
        <*> fmap fst (generateFormPost
                (createOrderForm wrapId ratioId exdir))
        <*> fmap fst (generateFormPost
                (createOrderForm modalWrapId modalRatioId exdir))

getCommonData :: HandlerFor App ([(Text, Html)], Maybe (Key User))
getCommonData = do
    mUser <- maybeClientUser
    (,) <$> getMessages <*> pure mUser


getActiveOrders :: Maybe UserId -> Handler ([Entity ExchangeOrder], [Entity ExchangeOrder])
getActiveOrders mu = do
    let userConstraint = case mu of
            Nothing -> []
            Just _  -> []
    os <- runDB $ selectList
        ((ExchangeOrderIsActive ==. True) : userConstraint)
        [Asc ExchangeOrderNormalizedRatio, Asc ExchangeOrderCreated]
    return $ partition (isPzmRubOrder . entityVal) os
    where isPzmRubOrder = (== ExchangePzmRur) . exchangeOrderPair

-- * News

featuredModal :: Widget
featuredModal = do
    mayFeatured <- handlerToWidget getLastFeaturedNews
    case mayFeatured of
        Nothing -> [whamlet||]
        Just (Entity iid info)  -> do
            let description = case infoDescHtml info of
                    Just ""    -> infoContentHtml info
                    Just desc' -> desc'
                    _          -> infoContentHtml info
            wrapId     <- newIdent
            proj       <- appType . appSettings <$> getYesod
            cookieName <- appHiddenNewsCookieName . appSettings <$> getYesod
            let (hostName, _) = projectNameHost proj
            $(widgetFile "modal/featured-news")

getLastFeaturedNews :: Handler (Maybe (Entity Info))
getLastFeaturedNews = do
    allNews <- runDB $ selectList [ InfoFeatured ==. True ] [ Desc InfoCreated, LimitTo 1 ]
    case allNews of
        []  -> return Nothing
        x:_ -> return (Just x)

-- * Depth of Market

renderDomTable :: ExchangePair -> Bool -> Bool -> DOMStats -> Widget
renderDomTable p buy hidden d = domDivView pair' hidden title body
  where
    pair' = if buy then flipPair p else p

    pairStats =
        sortRateAsc . HMS.toList <$> HMS.lookup pair' d

    title = if buy
        then [shamlet|#{currencySymbol inc} ⇢ #{currencySymbol outc}|]
        else [shamlet|#{currencySymbol outc} ⇢ #{currencySymbol inc}|]

    body =
        fmap (\(buyTotal, sellTotal, widget) -> widget buyTotal sellTotal) rows

    rows = foldRows <$> pairStats

    (outc, inc) = unPairCurrency p

    foldRows :: [(Double, (Int, Int, Int))] -> (Int, Int, Int -> Int -> Widget)
    foldRows = foldr step (0, 0, (\_ _ -> mempty))

    step (r, s@(_, sOut, sIn)) (n_buy, n_sell, w) =
        let n_buy' = n_buy + sIn
            n_sell' = n_sell + sOut
        in (n_buy', n_sell', (\tb ts ->
                let depth = if buy then tb - n_buy else n_sell'
                    total = if buy then tb else ts
                in domDivRow r depth total buy s >> w tb ts)
                )

    sortRateAsc = sortBy (flip (comparing fst))

domDivView :: ExchangePair -> Bool -> Html -> Maybe Widget -> Widget
domDivView pair' hidden title mbody  =
    let (outc, inc) = unPairCurrency pair'
        expair = intercalate "_" . map (toLower . currencyCodeT) $ [outc, inc]
        body = fromMaybe (emptyList 10 4) mbody
    in $(widgetFile "dom/divs/view")

domDivRow :: Double -> Int -> Int -> Bool -> DOMRateStats -> Widget
domDivRow r depth t buy d =
    let (ordersCount, outCents, inCents) = d
        dLeft = "left" :: Text
        dRight = "right" :: Text
        (direction, color) = if buy
            then (dLeft, "#47b9002b")
            else (dRight, "#ff23002b")
        barWidth :: Double
        barWidth = fromIntegral (depth * oneCoinCents) / fromIntegral t
        tWidth :: Int
        tWidth = round barWidth
        fWidth = 100 - tWidth
        style = concat
            [ "background: linear-gradient(to "
            , direction <> ", "
            , "#fff0 " <> (pack . show $ fWidth) <> "%, "
            , color <> " " <> (pack . show $ fWidth) <> "%);" ]
    in $(widgetFile "dom/divs/row")

clickableOrderW :: Text -> Widget
clickableOrderW wrapId = $(widgetFile "dom/one-click-order")

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


-- * Exchange History

getLastExchangesOf ::
       ExchangePair
    -> Handler [(Entity ExchangeOrderExecution, Entity ExchangeOrder)]
getLastExchangesOf p = runDB . E.select . from $ \(o `InnerJoin` e) -> do
    on (o ^. ExchangeOrderId E.==. e ^. ExchangeOrderExecutionOrderId)
    where_ (o ^. ExchangeOrderPair `in_` valList [ p, flipPair p ])
    orderBy [desc (e ^. ExchangeOrderExecutionTime), asc (o ^. ExchangeOrderCreated)]
    limit 20
    return (e, o)

exchangeHistoryW ::
       [(ExchangePair, [(Entity ExchangeOrderExecution, Entity ExchangeOrder)])]
    -> ExchangePair
    -> Widget
exchangeHistoryW groups activePair = mapM_ itemWidget groups
  where
    itemWidget (p, exs) =
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

    body = mapM_ (exchangeW cOut cIn) exs

exchangeW ::
       Currency
    -> Currency
    -> (Entity ExchangeOrderExecution, Entity ExchangeOrder)
    -> Widget
exchangeW cOut cIn (Entity _ ex, Entity _ o) = do
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

    ratio' = exchangeOrderNormalizedRatio o

    r = if isAsk then ratio' else 1 / ratio'

    income = multiplyCents r transfer

    rate = fixedDoubleT 2 (exchangeOrderNormalizedRatio o)

    (outCents, inCents) = if isAsk
        then (transfer, income)
        else (income, transfer)

    typW = [whamlet|
        <span :isAsk:.ask :not isAsk:.bid>
            $if isAsk
                _{MsgExchangeAsk}
            $else
                _{MsgExchangeBid}
        |]

-- * Misc

maybeClientUser :: HandlerFor App (Maybe (Key User))
maybeClientUser = (entityKey . fst <$>) <$> maybeClient

selectPair :: Maybe Text -> Maybe Text -> ExchangePair
selectPair Nothing Nothing = defPairDir ExchangePzmRur
selectPair _ Nothing       = defPairDir ExchangePzmRur
selectPair Nothing _       = defPairDir ExchangePzmRur
selectPair (Just c1) (Just c2)
    | c1 == "rub" && c2 == "ouro" =
        defPairDir ExchangeRurOur
    | c2 == "rub" && c1 == "ouro" =
        defPairDir ExchangeOurRur
    | c1 == "pzm" && c2 == "ouro" =
        defPairDir ExchangePzmOur
    | c2 == "pzm" && c1 == "ouro" =
        defPairDir ExchangeOurPzm
    | otherwise = defPairDir ExchangePzmRur

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}
module Utils.Common where

import           Import.NoFoundation

import           Utils.Time

import           Data.Time.Format    ( TimeLocale (..) )


{- YESOD.  APP -}

setCompositeTitle
    :: (MonadWidget m, WidgetFor site ~ m, RenderMessage site msg)
    => [ msg ] -> m ()
setCompositeTitle ms = do
    r <- getMessageRender
    setTitle . toHtml . intercalate " | " . map r $ ms


{- TIME -}

dateTimeRowWM :: (WidgetFor s ~ m, MonadWidget m) => UTCTime -> m ()
dateTimeRowWM t = do
    fd <- getFormatDateRender
    ft <- getFormatTimeRender
    [whamlet|
        <span .text-uppercase>#{fd t}#
        &nbsp;&nbsp;#
        <span .text-muted>#{ft t}|]

getFormatDateRender :: MonadHandler m => m (UTCTime -> Text)
getFormatDateRender = (\(l, t) -> localeFormatDate l . offsetTime t)
    <$> getFormatParams

getFormatTimeRender :: MonadHandler m => m (UTCTime -> Text)
getFormatTimeRender = (\(l, t) -> localeFormatTime l . offsetTime t)
    <$> getFormatParams

getFormatParams :: MonadHandler m => m (TimeLocale, Int)
getFormatParams = (,)
    <$> selectLocale
    <*> timezoneOffsetFromCookie

selectLocale :: MonadHandler m => m TimeLocale
selectLocale = locale <$> languages
    where
    locale ("ru":_) = ruTimeLocale
    locale (_:rest) = locale rest
    locale []       = defaultTimeLocale

projectNameHost :: AppType -> (Text, Text)
projectNameHost typ = if typ == FenixApp
    then ("FENIX.TRADING", "FENIX.TRADING")
    else ("OutBirds", "OUTB.INFO")

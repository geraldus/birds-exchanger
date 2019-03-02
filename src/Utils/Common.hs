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

dateTimeRowW :: (WidgetFor s ~ m, MonadWidget m) => UTCTime -> m ()
dateTimeRowW t = do
    fd <- getFormatDateRender
    ft <- getFormatTimeRender
    [whamlet|#{ft t} #{fd t}|]

getFormatDateRender :: MonadWidget m => m (UTCTime -> Html)
getFormatDateRender = (\(l, t) -> localeFormatDate l . offsetTime t)
    <$> getFormatParams

getFormatTimeRender :: MonadWidget m => m (UTCTime -> Html)
getFormatTimeRender = (\(l, t) -> localeFormatTime l . offsetTime t)
    <$> getFormatParams

getFormatParams :: MonadWidget m => m (TimeLocale, Int)
getFormatParams = (,)
    <$> liftHandler selectLocale
    <*> liftHandler timezoneOffsetFromCookie

selectLocale :: MonadHandler m => m TimeLocale
selectLocale = locale <$> languages
    where
    locale ("ru":_) = ruTimeLocale
    locale (_:rest) = locale rest
    locale []       = defaultTimeLocale


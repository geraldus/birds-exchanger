module Utils.App.Common where

import           Import

import           Utils.Time

import           Data.Time.Format ( TimeLocale (..) )


getRenders :: WidgetFor App (Route App -> Text, AppMessage -> Text)
getRenders = (,) <$> liftHandler getUrlRender <*> liftHandler getMessageRender

dateTimeRowW :: UTCTime -> Widget
dateTimeRowW t = do
    fd <- getFormatDateRender
    ft <- getFormatTimeRender
    [whamlet|#{ft t} #{fd t}|]

getFormatDateRender :: WidgetFor App (UTCTime -> Html)
getFormatDateRender = (\(l, t) -> localeFormatDate l . offsetTime t)
    <$> getFormatParams

getFormatTimeRender :: WidgetFor App (UTCTime -> Html)
getFormatTimeRender = (\(l, t) -> localeFormatTime l . offsetTime t)
    <$> getFormatParams

getFormatParams :: WidgetFor App (TimeLocale, Int)
getFormatParams = (,)
    <$> liftHandler selectLocale
    <*> liftHandler timezoneOffsetFromCookie

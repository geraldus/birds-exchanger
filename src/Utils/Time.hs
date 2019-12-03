{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Utils.Time where

import           ClassyPrelude.Yesod

import           Data.Time.Clock
import           Data.Time.Format    ( TimeLocale (..) )
import           Data.Time.LocalTime ( TimeZone (..) )
import           Text.Read           ( readMaybe )


mayCookie :: MonadHandler m => Text -> Text -> m Text
mayCookie name defaultVal = fromMaybe defaultVal <$> lookupCookie name

-- | Read client timezone offset from @timezoneOffset@ cookie.
-- Return offset value in minutes.
timezoneOffsetFromCookie :: MonadHandler m => m Int
timezoneOffsetFromCookie =
    fromMaybe 0 . (readMaybe . unpack =<<) <$> lookupCookie "timezoneOffset"


renderTimeDateCol :: TimeLocale -> Int -> UTCTime -> Html
renderTimeDateCol loc moff utc = [shamlet|
    #{localeFormatTime loc utc'}<br>
    <small>#{localeFormatDate loc utc'}
    |]
  where utc' = offsetTime moff utc

renderDateRow :: TimeLocale -> Int -> UTCTime -> Html
renderDateRow loc moff utc = [shamlet|
    <span>
        #{toHtml gencase}
    |]
  where utc' = offsetTime moff utc
        ft = formatTime loc "%e %B %Y" utc'
        gencase = unwords . map genitiveCase $ words ft

renderDateTimeRow :: TimeLocale -> Int -> UTCTime -> Html
renderDateTimeRow loc moff utc = [shamlet|
    <span .text-uppercase>
        #{localeFormatDate loc utc'}#
        &#32;&#32;#{localeFormatTime loc utc'}
    |]
  where utc' = offsetTime moff utc


-- | Locale representing Russian free-form usage.
-- Note that the parsing functions will regardless parse "UTC", single-letter
-- military time-zones, and +HHMM format.
ruTimeLocale :: TimeLocale
ruTimeLocale = TimeLocale
    { wDays          = [ ("Воскресенье", "Вс")
                       , ("Понедельник", "Пн")
                       , ("Вторник"    , "Вт")
                       , ("Среда"      , "Ср")
                       , ("Четверг"    , "Чт")
                       , ("Пятница"    , "Пт")
                       , ("Суббота"    , "Сб")
                       ]
    , months         = [ ("Январь"  , "янв")
                       , ("Февраль" , "фев")
                       , ("Март"    , "мар")
                       , ("Апрель"  , "апр")
                       , ("Май"     , "мая")
                       , ("Июнь"    , "июн")
                       , ("Июль"    , "июл")
                       , ("Август"  , "авг")
                       , ("Сентябрь", "сен")
                       , ("Октябрь" , "окт")
                       , ("Ноябрь"  , "ноя")
                       , ("Декабрь" , "дек")
                       ]
    , amPm           = ("ДП", "ПП")
    , dateTimeFmt    = "%a, %e %b %Y %H:%M:%S %Z"
    , dateFmt        = "%d.%m.%Y"
    , timeFmt        = "%H:%M:%S"
    , time12Fmt      = "%I:%M:%S %p"
    , knownTimeZones = [ TimeZone 0         False "UT"
                       , TimeZone 0         False "GMT"
                       , TimeZone (2 * 60)  False "EET"
                       , TimeZone (3 * 60)  False "MSK"
                       , TimeZone (4 * 60)  False "SAMT"
                       , TimeZone (5 * 60)  False "YEKT"
                       , TimeZone (6 * 60)  False "OMST"
                       , TimeZone (7 * 60)  False "KRAT"
                       , TimeZone (7 * 60)  False "NOVT"
                       , TimeZone (8 * 60)  False "IRKT"
                       , TimeZone (9 * 60)  False "YAKT"
                       , TimeZone (10 * 60) False "VLAT"
                       , TimeZone (11 * 60) False "MAGT"
                       , TimeZone (11 * 60) False "SAKT"
                       , TimeZone (11 * 60) False "SRET"
                       , TimeZone (12 * 60) False "ANAT"
                       , TimeZone (12 * 60) False "PETT"
                       ]
    }

dateFormatMNH :: String
dateFormatMNH = "%e %b %Y"

localeFormatTime :: TimeLocale -> UTCTime -> Html
localeFormatTime l = toHtml . formatTime l (timeFmt l)

localeFormatDate :: TimeLocale -> UTCTime -> Html
localeFormatDate l = toHtml . formatTime l dateFormatMNH

offsetTime :: Int -> UTCTime -> UTCTime
offsetTime minutesOffset = addUTCTime (negate . fromIntegral $ minutesOffset * 60)

genitiveCase :: String -> String
genitiveCase t
    | t == "Январь"   = "Января"
    | t == "Февраль"  = "Февраля"
    | t == "Март"     = "Марта"
    | t == "Апрель"   = "Апреля"
    | t == "Май"      = "Мая"
    | t == "Июнь"     = "Июня"
    | t == "Июль"     = "Июля"
    | t == "Август"   = "Августа"
    | t == "Сентябрь" = "Сентября"
    | t == "Октябрь"  = "Октября"
    | t == "Ноябрь"   = "Ноября"
    | t == "Декабрь"  = "Декабря"
    | otherwise = t

utcDayWithTimeZoneAdded :: NominalDiffTime -> (a -> UTCTime) -> Entity a -> Day
utcDayWithTimeZoneAdded offsetSeconds getDate =
    utctDay
    . addUTCTime offsetSeconds
    . getDate
    . entityVal

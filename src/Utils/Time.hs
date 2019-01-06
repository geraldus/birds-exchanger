module Utils.Time
    ( ruTimeLocale
    )
where

import           Data.Time.Format               ( TimeLocale(..) )
import           Data.Time.LocalTime            ( TimeZone(..) )
import           ClassyPrelude.Yesod

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
    , months         = [ ("Январь"  , "Янв")
                       , ("Февраль" , "Фев")
                       , ("Март"    , "Мар")
                       , ("Апрель"  , "Апр")
                       , ("Май"     , "Май")
                       , ("Июнь"    , "Июн")
                       , ("Июль"    , "Июл")
                       , ("Август"  , "Авг")
                       , ("Сентябрь", "Сен")
                       , ("Октябрь" , "Окт")
                       , ("Ноябрь"  , "Ноя")
                       , ("Декабрь" , "Дек")
                       ]
    , amPm           = ("ДП", "ПП")
    , dateTimeFmt    = "%a, %e %b %Y %H:%M:%S %Z"
    , dateFmt        = "%d.%m.%y"
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
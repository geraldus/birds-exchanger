{-# LANGUAGE DeriveGeneric #-}
module Local.Persist.Notice where

import           ClassyPrelude.Yesod
import           Data.Aeson


data NoticeType
    = NoticeEmail
    -- | NoticeSMS
    -- | NoticeTelegram
    -- | NoticeVK
    -- | ...
    deriving (Generic, Show, Read, Eq)
derivePersistField "NoticeType"

instance ToJSON NoticeType where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON NoticeType

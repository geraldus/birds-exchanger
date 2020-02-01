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


data NoticeSubject
    = NoticeSubjectUndefined
    | NoticeSubjectQuickRegistation
    | NoticeSubjectEmailVerification
    | NoticeSubjectRegistration
    | NoticeSubjectPasswordReset
    | NoticeSubjectOperatorStocksPurchaseConfirmed
    deriving (Generic, Show, Read, Eq)
derivePersistField "NoticeSubject"

instance ToJSON NoticeSubject where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON NoticeSubject

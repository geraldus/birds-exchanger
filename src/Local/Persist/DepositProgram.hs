{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Local.Persist.DepositProgram where

import           ClassyPrelude.Yesod

import           Data.Aeson


data RateType
    = SimplePercent
    | CompoundPercent
    deriving (Generic, Show, Read, Eq)
derivePersistField "RateType"

instance ToJSON RateType where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON RateType

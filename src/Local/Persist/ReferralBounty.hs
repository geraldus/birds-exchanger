{-# LANGUAGE DeriveGeneric #-}
module Local.Persist.ReferralBounty where

import           ClassyPrelude.Yesod
import           Data.Aeson


data ReferralBountyType
    = RegistrationBounty
    deriving (Generic, Show, Read, Eq)
derivePersistField "ReferralBountyType"

instance ToJSON ReferralBountyType where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ReferralBountyType
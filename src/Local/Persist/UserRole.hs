{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Local.Persist.UserRole where


import           Data.Aeson
import           Database.Persist.TH
import           GHC.Generics        ( Generic (..) )
import           Prelude


data UserRole = Admin | Editor | Operator | MaoOperator| Client
    deriving (Show, Read, Eq, Generic)
instance FromJSON UserRole
instance ToJSON UserRole
instance ToJSONKey UserRole
derivePersistField "UserRole"

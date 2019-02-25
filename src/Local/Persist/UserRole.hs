{-# LANGUAGE TemplateHaskell #-}
module Local.Persist.UserRole where


import Database.Persist.TH
import Prelude


data UserRole = Admin | Editor | Operator | Client
    deriving (Show, Read, Eq)
derivePersistField "UserRole"
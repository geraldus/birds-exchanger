{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Model where

import           ClassyPrelude.Yesod

import           Local.Persist.Currency
import           Local.Persist.Exchange
import           Local.Persist.Notice
import           Local.Persist.TransferMethod
import           Local.Persist.UserRole
import           Local.Persist.Wallet

import           Database.Persist.Quasi
import           Database.Persist.Sql         ( fromSqlKey )


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance ToJSON (Entity User) where
    toJSON (Entity uid (User ident _ role)) = object
        [ "id"    .= fromSqlKey uid
        , "ident" .= toJSON ident
        , "role"  .= toJSON role ]

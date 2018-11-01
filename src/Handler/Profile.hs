{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Profile where

import Import
import Type.Auth.SuperUser (SuperUser (..))


getProfileR :: Handler Html
getProfileR = do
    (_, authUser) <- requireAuthPair
    userName <- return $ case authUser of
        Left user -> userIdent user
        Right su  -> suName su
    defaultLayout $ do
        setTitle . toHtml $ userName <> "'s User page"
        $(widgetFile "profile")

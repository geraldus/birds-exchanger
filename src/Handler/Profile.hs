{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Profile where

import           Import
import           Type.Auth.SuperUser            ( SuperUser(..) )


getProfileR :: Handler Html
getProfileR = do
    userName <- userNameF . snd <$> requireAuthPair
    defaultLayout $ do
        setTitle . toHtml $ "Мой портфель | " <> userName
        $(widgetFile "profile")
  where
    userNameF (Left  u) = userIdent u
    userNameF (Right u) = suName u

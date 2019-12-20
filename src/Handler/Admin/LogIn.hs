{-# LANGUAGE OverloadedStrings #-}
module Handler.Admin.LogIn where

import           Import


getAdminLogInR :: Handler Html
getAdminLogInR = do
    ma <- maybeAuthId
    -- tp <- getRouteToParent :: m ~ App  => HandlerFor App (Route (SubHandlerSite m) -> Route (HandlerSite m))
    when (isJust ma) (redirect HomeR)
    defaultLayout $ do
        setTitle "Вход"
        $(widgetFile "admin/login")


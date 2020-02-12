{-# LANGUAGE OverloadedStrings #-}
module Handler.Operator.LogIn where

import           Import


getOperatorLogInR :: Handler Html
getOperatorLogInR = do
    ma <- maybeAuthId
    -- tp <- getRouteToParent :: m ~ App  => HandlerFor App (Route (SubHandlerSite m) -> Route (HandlerSite m))
    when (isJust ma) (redirect HomeR)
    defaultLayout $ do
        setTitle "Вход"
        $(widgetFile "operator/login")

{-# LANGUAGE OverloadedStrings #-}
module Handler.Operator.LogIn where


import           Import
import           Local.Auth


getOperatorLogInR :: Handler Html
getOperatorLogInR = do
    ma <- maybeAuthId
    -- tp <- getRouteToParent :: m ~ App  => HandlerFor App (Route (SubHandlerSite m) -> Route (HandlerSite m))
    when (isJust ma) (redirect HomeR)
    defaultLayout $ do
        setTitle "Вход | OutBirds"
        $(widgetFile "operator/login")

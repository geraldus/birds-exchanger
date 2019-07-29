module Handler.Client.Settings where

import           Import


getClientSettingsR :: Handler Html
getClientSettingsR = defaultLayout $
    $(widgetFile "client/settings")

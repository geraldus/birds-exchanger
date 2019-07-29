module Handler.Client.Settings where

import           Form.Client.PersonalData
import           Import


getClientSettingsR :: Handler Html
getClientSettingsR = do
    clientData <- requireClientData
    ((_, formWidget), enctype) <- generateFormGet personalDataForm
    defaultLayout $
        $(widgetFile "client/settings")

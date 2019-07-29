module Handler.Client.Settings where

import           Form.Client.PersonalData
import           Import


getClientSettingsR :: Handler Html
getClientSettingsR = do
    (user, _) <- requireClientData
    ((_, formWidget), enctype) <- generateFormGet personalDataForm
    defaultLayout $
        $(widgetFile "client/settings")

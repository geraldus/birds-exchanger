{-# LANGUAGE OverloadedStrings #-}
module Handler.Client.Settings where

import           Form.Client.PersonalData
import           Import


getClientSettingsR :: Handler Html
getClientSettingsR = do
    (user, _) <- requireClientData
    -- changePasswordElementRoot <- newIdent
    let passwordPrefix = take 6 <$> (userPassword . entityVal) user
    let shouldNoticeAboutPasswordChange = passwordPrefix /= Just "sha256"
    defaultLayout
        $(widgetFile "client/settings")

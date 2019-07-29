{-# LANGUAGE OverloadedStrings #-}
module Form.Client.PersonalData where

import           Import


data PersonalData = PersonalData
    { pdLastName   :: Text
    , pdFirstName  :: Text
    , pdPatronymic :: Text
    }

personalDataForm :: Form PersonalData
personalDataForm extra = do
    let widget = [whamlet|#{extra}|]
    return (pure (PersonalData "1" "2" "3"), widget)

{-# LANGUAGE OverloadedStrings #-}
module Handler.API.App where

import           Import
import           Paths_prizm_exchange ( version )

import           Data.Version         ( showVersion )


getApiAppConfigR :: Handler TypedContent
getApiAppConfigR = do
    auth <- maybeClient
    let authMeta = case auth of
            Nothing -> object [ "guest" .= True ]
            Just _ -> object [ "guest" .= False ]
    let appVersion = showVersion version
    selectRep . provideRep . pure . toJSON $ object
        [ "auth" .= authMeta
        , "app" .= object
            [ "version" .= appVersion ]
        , "nav" .= array  ([] :: [Value])
        ]

{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
module Handler.API.App where

import           Import
import           Paths_prizm_exchange ( version )

import           Data.Version         ( showVersion )

default (Text)


guestNav renderUrl renderMessage = array
    [ object
        [ "url" .= renderUrl HomeR
        , "label" .= renderMessage MsgMenuTitleHome
        , "tags" .= array [ "guest" ]
        ]
    , object
        [ "url" .= renderUrl InfoListR
        , "label" .= renderMessage MsgMenuTitleNews
        , "tags" .= array [ "guest" ]
        ]
    , object
        [ "url" .= renderUrl TermsOfUseR
        , "label" .= renderMessage MsgMenuTitleTermsOfUse
        , "tags" .= array [ "guest" ]
        ]
    , object
        [ "url" .= renderUrl SignUpR
        , "label" .= renderMessage MsgMenuTitleSignUp
        , "tags" .= array [ "guest", "auth" ]
        ]
    , object
        [ "url" .= renderUrl (AuthR LoginR)
        , "label" .= renderMessage MsgMenuTitleSignIn
        , "tags" .= array [ "guest", "auth" ]
        ]
    ]


getApiAppConfigR :: Handler TypedContent
getApiAppConfigR = do
    auth <- maybeClient
    url <- getUrlRender
    message <- getMessageRender
    let authMeta = case auth of
            Nothing -> object [ "guest" .= True ]
            Just _  -> object [ "guest" .= False ]
    let appVersion = showVersion version
    selectRep . provideRep . pure . toJSON $ object
        [ "auth" .= authMeta
        , "app" .= object
            [ "version" .= object
                [ "number" .= appVersion
                , "extraLabel" .= message MsgVerPublicBeta
                ]
            ]
        , "nav" .= guestNav url message
        ]

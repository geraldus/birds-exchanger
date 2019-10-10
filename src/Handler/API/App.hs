{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
module Handler.API.App where

import           Import
import           Paths_prizm_exchange ( version )

import           Data.Version         ( showVersion )

default (Text)


guestNav renderUrl renderMessage =
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

userNav renderUrl renderMessage =
    [ object
        [ "url" .= renderUrl (AuthR LogoutR)
        , "label" .= renderMessage MsgMenuTitleSignOut
        , "tags" .= array [ "user", "auth" ]
        ]
    , object
        [ "url" .= renderUrl ProfileR
        , "label" .= renderMessage MsgMenuTitleClientHistory
        , "tags" .= array [ "user", "history", "account", "index" ]
        ]
    , object
        [ "url" .= renderUrl ClientOrdersR
        , "label" .= renderMessage MsgMenuTitleClientOrders
        , "tags" .= array [ "user", "history", "orders", "index" ]
        ]
    , object
        [ "url" .= renderUrl DepositR
        , "label" .= renderMessage MsgMenuTitleClientDeposit
        , "tags" .= array [ "user", "history", "deposit", "index" ]
        ]
    , object
        [ "url" .= renderUrl WithdrawalR
        , "label" .= renderMessage MsgMenuTitleClientWithdrawal
        , "tags" .= array [ "user", "history", "withdrawal", "index" ]
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
        , "nav" .= array (guestNav url message ++ userNav url message)
        ]

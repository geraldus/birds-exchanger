{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
module Handler.API.App where

import           Import
import           Paths_prizm_exchange ( version )

import           Data.Version         ( showVersion )

default (Text)


tag_guest :: Text
tag_guest = "guest" :: Text
tag_auth :: Text
tag_auth = "auth" :: Text
tag_user :: Text
tag_user = "user" :: Text

guestNav :: (Route App -> Text) -> (AppMessage -> Text) -> [Value]
guestNav renderUrl message =
    [ object
        [ "url" .= renderUrl HomeR
        , "label" .= message MsgMenuTitleHome
        , "tags" .= array [ tag_guest ]
        ]
    , object
        [ "url" .= renderUrl InfoListR
        , "label" .= message MsgMenuTitleNews
        , "tags" .= array [ tag_guest ]
        ]
    , object
        [ "url" .= renderUrl TermsOfUseR
        , "label" .= message MsgMenuTitleTermsOfUse
        , "tags" .= array [ tag_guest ]
        ]
    , object
        [ "url" .= renderUrl SignUpR
        , "label" .= message MsgMenuTitleSignUp
        , "tags" .= array [ tag_guest, tag_auth ]
        ]
    , object
        [ "url" .= renderUrl (AuthR LoginR)
        , "label" .= message MsgMenuTitleSignIn
        , "tags" .= array [ tag_guest, tag_auth ]
        ]
    ]

userNav :: (Route App -> Text) -> (AppMessage -> Text) -> [Value]
userNav renderUrl message =
    [ object
        [ "url" .= renderUrl (AuthR LogoutR)
        , "label" .= message MsgMenuTitleSignOut
        , "tags" .= array [ tag_user, tag_auth ]
        ]
    , object
        [ "url" .= renderUrl ProfileR
        , "label" .= message MsgMenuTitleClientHistory
        , "tags" .= array [ tag_user, "history", "account", "index" ]
        ]
    , object
        [ "url" .= renderUrl ClientOrdersR
        , "label" .= message MsgMenuTitleClientOrders
        , "tags" .= array [ tag_user, "history", "orders", "index" ]
        ]
    , object
        [ "url" .= renderUrl DepositR
        , "label" .= message MsgMenuTitleClientDeposit
        , "tags" .= array [ tag_user, "history", "deposit", "index" ]
        ]
    , object
        [ "url" .= renderUrl WithdrawalR
        , "label" .= message MsgMenuTitleClientWithdrawal
        , "tags" .= array [ tag_user, "history", "withdrawal", "index" ]
        ]
    ]


getApiAppConfigR :: Handler TypedContent
getApiAppConfigR = do
    auth <- maybeClient
    url <- getUrlRender
    message <- getMessageRender
    let (authMeta, navExtra) = case auth of
            Nothing -> (object [ "guest" .= True ], [])
            Just _  -> (object [ "guest" .= False ], userNav url message)
    let nav = array $ guestNav url message ++ navExtra
    let appVersion = showVersion version
    selectRep . provideRep . pure . toJSON $ object
        [ "auth" .= authMeta
        , "app" .= object
            [ "version" .= object
                [ "number" .= appVersion
                , "extraLabel" .= message MsgVerPublicBeta
                ]
            ]
        , "nav" .= nav
        ]


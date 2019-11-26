{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Handler.API.App where

import           Paths_prizm_exchange   ( version )

import           Import
import           Local.Persist.Currency ( currencyCodeT )
import           Local.Persist.Exchange ( ExchangePair (..) )
import           Market.Functions       ( foldMarketOrders )
import           Market.Type
import           Utils.Database.Orders  ( selectActiveOrdersOf )
import           Utils.Money            ( unPairCurrency )

import           Data.Aeson
import qualified Data.HashMap.Strict    as HMS
import           Data.Version           ( showVersion )
import           Yesod.WebSockets

default (Text)


tagGuest :: Text
tagGuest = "guest" :: Text
tagAuth :: Text
tagAuth = "auth" :: Text
tagUser :: Text
tagUser = "user" :: Text

guestNav :: (Route App -> Text) -> (AppMessage -> Text) -> [Value]
guestNav renderUrl message =
    [ object
        [ "url" .= renderUrl HomeR
        , "label" .= message MsgMenuTitleHome
        , "tags" .= array [ tagGuest ]
        ]
    , object
        [ "url" .= renderUrl InfoListR
        , "label" .= message MsgMenuTitleNews
        , "tags" .= array [ tagGuest ]
        ]
    , object
        [ "url" .= renderUrl TermsOfUseR
        , "label" .= message MsgMenuTitleTermsOfUse
        , "tags" .= array [ tagGuest ]
        ]
    , object
        [ "url" .= renderUrl SignUpR
        , "label" .= message MsgMenuTitleSignUp
        , "tags" .= array [ tagGuest, tagAuth ]
        ]
    , object
        [ "url" .= renderUrl (AuthR LoginR)
        , "label" .= message MsgMenuTitleSignIn
        , "tags" .= array [ tagGuest, tagAuth ]
        ]
    ]

userNav :: (Route App -> Text) -> (AppMessage -> Text) -> [Value]
userNav renderUrl message =
    [ object
        [ "url" .= renderUrl (AuthR LogoutR)
        , "label" .= message MsgMenuTitleSignOut
        , "tags" .= array [ tagUser, tagAuth ]
        ]
    , object
        [ "url" .= renderUrl ProfileR
        , "label" .= message MsgMenuTitleClientHistory
        , "tags" .= array [ tagUser, "history", "account", "index" ]
        ]
    , object
        [ "url" .= renderUrl ClientOrdersR
        , "label" .= message MsgMenuTitleClientOrders
        , "tags" .= array [ tagUser, "history", "orders", "index" ]
        ]
    , object
        [ "url" .= renderUrl DepositR
        , "label" .= message MsgMenuTitleClientDeposit
        , "tags" .= array [ tagUser, "history", "deposit", "index" ]
        ]
    , object
        [ "url" .= renderUrl WithdrawalR
        , "label" .= message MsgMenuTitleClientWithdrawal
        , "tags" .= array [ tagUser, "history", "withdrawal", "index" ]
        ]
    ]


getApiAppConfigR :: Handler TypedContent
getApiAppConfigR = do
    auth <- maybeClient
    url <- getUrlRender
    message <- getMessageRender
    dom <- appDOM <$> getYesod >>= atomically . readTMVar
    let defaultPairs = [ ExchangePzmRur, ExchangeOurRur, ExchangeOurPzm ]
    activePairOrders <- concat <$> runDB (mapM selectActiveOrdersOf defaultPairs)
    let domStats = foldMarketOrders defaultPairs activePairOrders
    let authMeta = maybe
            (object [ "guest" .= True ])
            (const . object $ [ "guest" .= False ])
            auth
    let appVersion = showVersion version
    selectRep . provideRep . pure . toJSON $ object
        [ "auth" .= authMeta
        , "app" .= object
            [ "version" .= object
                [ "number" .= appVersion
                , "extraLabel" .= message MsgVerPublicBeta
                ]
            ]
        , "dom" .= toJsonFullMarketDom domStats
        ]

notificationSocket :: WebSocketsT Handler ()
notificationSocket =
    webSocketJsonMessage ("Notification web socket" :: Text)


getNotificationSocketR :: Handler ()
getNotificationSocketR = webSockets notificationSocket


webSocketJsonMessage :: ToJSON a => a -> WebSocketsT Handler ()
webSocketJsonMessage = sendTextData . decodeUtf8 . encode . toJSON


cleanDomJson :: DOMStats -> Value
cleanDomJson stats = toJSON cleanedDomJson
    where
        mappings = HMS.toList stats
        unpaired = flip map mappings $ \(p, dom) ->
            let (from, to) = unPairCurrency p
            in (from, to, dom)
        cleanedDomJson = flip map unpaired $ \(from, to, dom) ->
            object [ "from" .= currencyCodeT from
                   , "to" .= currencyCodeT to
                   , "dom" .= toJSON (cleanDomRowJson dom) ]

cleanDomRowJson :: DOMStatsRateMap -> Value
cleanDomRowJson pairDom = toJSON cleanedRowJson
  where
    mappings = HMS.toList pairDom
    cleanedRowJson = flip map mappings $ \(rate, (cnt, outAmt, inAmt)) ->
        object [ "rate" .= rate
               , "count" .= cnt
               , "out" .= outAmt
               , "in" .= inAmt ]

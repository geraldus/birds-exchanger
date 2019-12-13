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


getApiAppConfigR :: Handler TypedContent
getApiAppConfigR = do
    auth <- maybeClient
    message <- getMessageRender
    _marketDepth <- appDOM <$> getYesod >>= atomically . readTMVar
    let defaultPairs = [ ExchangePzmRur, ExchangeOurRur, ExchangeOurPzm ]
    activePairOrders <- concat <$> runDB (mapM selectActiveOrdersOf defaultPairs)
    let domStats = foldMarketOrders defaultPairs activePairOrders
    let authMeta = maybe
            (object [ "guest" .= True ])
            (\(Entity uid user, _) -> object $
                [ "guest" .= False
                , "ident" .= userIdent user
                , "id" .= toJSON uid
                ])
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

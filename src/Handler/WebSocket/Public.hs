{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
module Handler.WebSocket.Public where

import           Import

import qualified Data.Aeson       as A
import           Yesod.WebSockets

default (Text, String)


publicNotificationsSocket :: WebSocketsT Handler ()
publicNotificationsSocket = do
    c <- appChannelsPublicNotifications . appChannels <$> getYesod
    rc <- liftIO . atomically $ dupTChan c
    forever $ do
        noticeJSON <- liftIO . atomically $ readTChan rc
        send' $ noticeToJSON noticeJSON
  where
    send' = sendTextData . decodeUtf8 . A.encode

getPublicNotificationsWebSocketR :: Handler Html
getPublicNotificationsWebSocketR =
    webSockets publicNotificationsSocket >> notFound


noticeToJSON :: (ToJSON j, FromJSON j) => j -> A.Value
noticeToJSON n = A.object
  [ "object" A..= ("notification" :: Text)
  , "contents" A..= toJSON n ]

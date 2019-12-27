{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
module Handler.WebSocket.Client where

import           Import

import qualified Data.Aeson       as A
import           Yesod.WebSockets

default (Text, String)


clientNotificationsSocket :: WebSocketsT Handler ()
clientNotificationsSocket = do
    client <- lift requireClientId
    c <- appChannelsClientNotifications . appChannels <$> getYesod
    rc <- liftIO . atomically $ dupTChan c
    forever $ do
        (addressee, noticeJSON) <- liftIO . atomically $ readTChan rc
        when (addressee == client) $
            send' $ noticeToJSON noticeJSON
  where
    send' = sendTextData . decodeUtf8 . A.encode

getClientNotificationsWebSocketR :: Handler Html
getClientNotificationsWebSocketR =
    webSockets clientNotificationsSocket >> notFound


noticeToJSON :: (ToJSON j, FromJSON j) => j -> A.Value
noticeToJSON n = A.object
  [ "object" A..= ("notification" :: Text)
  , "contents" A..= toJSON n ]

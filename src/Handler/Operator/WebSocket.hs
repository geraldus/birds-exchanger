{-# LANGUAGE OverloadedStrings #-}
module Handler.Operator.WebSocket where

import           Import

import           Yesod.WebSockets


operatorSocket :: WebSocketsT Handler ()
operatorSocket = do
    sendTextData ("Welcome Operator!" :: Text)


getOperatorWebSocketR :: Handler Html
getOperatorWebSocketR = webSockets operatorSocket >> notFound

{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
module Handler.Operator.WebSocket where

import           Import

import qualified Data.Aeson       as A
import           Yesod.WebSockets

default (Text, String)


operatorSocket :: WebSocketsT Handler ()
operatorSocket = do
    sendTextData ("Welcome Operator!" :: Text)
    channels <- appChannels <$> getYesod
    let bcDepositConfirm = depositUserConfirm channels
    let bcWithdrawalRequest = withdrawalRequest channels
    dcReadChan <- liftIO . atomically $ dupTChan bcDepositConfirm
    wrReadChan <- liftIO . atomically $ dupTChan bcWithdrawalRequest
    forever $ race_
        (do
            dc <- liftIO . atomically $ readTChan dcReadChan
            send' $ typedUpdateJson "Deposit User Confirmation" dc)
        (do
            wr <- liftIO . atomically $ readTChan wrReadChan
            send' $ typedUpdateJson "Withdrawal User Request" wr)
  where
    send' = sendTextData . decodeUtf8 . A.encode

getOperatorWebSocketR :: Handler Html
getOperatorWebSocketR = webSockets operatorSocket >> notFound


typedUpdateJson :: (ToJSON j, FromJSON j) => Text -> j -> A.Value
typedUpdateJson typ n = A.object
  [ "type" A..= "update"
  , "object" A..= typ
  , "value" A..= toJSON n ]

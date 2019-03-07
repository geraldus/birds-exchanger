{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
module Handler.SuperUser.WebSocket where

import           Import                 hiding ( count, groupBy, on, (==.),
                                          (||.) )

import           Local.Persist.Currency
import           Local.Persist.UserRole
import           Local.Persist.Wallet
import           Yesod.WebSockets

import qualified Data.Aeson             as A
import           Database.Esqueleto

default (Text, String)


getSuperUserWebSocketR :: Handler Html
getSuperUserWebSocketR =
    webSockets superUserWebSocket >> notFound


superUserWebSocket :: WebSocketsT Handler ()
superUserWebSocket = do
    sendTextData ("Welcome Super User!" :: Text)
    forever $ do
        t <- receiveData :: WebSocketsT Handler Text
        case t of
            "user count" -> liftHandler getUsersCount >>=
                send' . countEventToJson "User Count"
            "inner profit stats" -> liftHandler getInnerProfit
                    >>= pure . (map (\(x,y) -> (currencyCodeT x, y)))
                    >>= send' . countsEventToJson "Inner Profit"
            "active deposit count" -> liftHandler getActiveDepositCount >>=
                send' . countEventToJson "Active Deposit Count"
            "accepted deposit count" -> liftHandler getAcceptedDepositCount >>=
                send' . countEventToJson "Accepted Deposit Count"
            _            -> sendTextData t
  where send' = sendTextData . decodeUtf8 . A.encode


data CountEvent = CountEvent
  { ceObjectType :: Text
  , ceValue      :: Int }

countEventToJson :: Text -> Int -> A.Value
countEventToJson typ n = A.object
  [ "type" A..= "count-event"
  , "object" A..= typ
  , "value" A..= n ]

countsEventToJson :: Text -> [(Text, Int)] -> A.Value
countsEventToJson typ vs = A.object
  [ "type" A..= "count-event"
  , "object" A..= typ
  , "value" A..= object (map (uncurry (A..=)) vs) ]


getUsersCount :: Handler Int
getUsersCount =
    fmap take1st <$> runDB $ select $ from (\u -> do
        where_ (u ^. UserRole ==. val Client)
        return $ count (u ^. UserId))


getActiveOrdersCount :: Handler Int
getActiveOrdersCount = error "!23"

getOrderExecutionsCount :: Handler Int
getOrderExecutionsCount = error "123"


getInnerProfit :: Handler [(Currency, Int)]
getInnerProfit = fmap (map (\(a, b) -> (unValue a, unValue b)))
    <$> runDB $ select $ from (\ip -> do
            let c = ip ^. InnerProfitRecordCurrency
            groupBy c
            return $ (c, countRows))


getActiveDepositList :: Handler [ Entity DepositRequest ]
getActiveDepositList = runDB $ select $ from $ \d -> do
    let s = d ^. DepositRequestStatus
    where_ (s ==. val New ||. s ==. val ClientConfirmed)
    return d

getActiveDepositCount :: Handler Int
getActiveDepositCount = fmap take1st <$> (runDB . select . from) $ \d -> do
    let s = d ^. DepositRequestStatus
    where_ (s ==. val New ||. s ==. val ClientConfirmed)
    return (count $ d ^. DepositRequestId)

getAcceptedDepositCount :: Handler Int
getAcceptedDepositCount = fmap take1st <$> (runDB . select . from) $
    \(d `InnerJoin` a) -> do
        on (d ^. DepositRequestId ==. a ^. AcceptedDepositDepositRequestId)
        -- on (d ^. ((DepositRequestId ==. a) . AcceptedDepositDepositRequestId))
        return (count $ d ^. DepositRequestId)

getActiveWithdrawalCount :: Handler Int
getActiveWithdrawalCount = error " 123"

getAcceptedWithdrawalCount :: Handler Int
getAcceptedWithdrawalCount = error "123"

take1st :: Num p => [Database.Esqueleto.Value p] -> p
take1st []    = 0
take1st (x:_) = unValue x

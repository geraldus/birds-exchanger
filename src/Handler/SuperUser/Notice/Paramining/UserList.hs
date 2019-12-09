{-# LANGUAGE OverloadedStrings #-}
module Handler.SuperUser.Notice.Paramining.UserList where

import           Import                 hiding ( Value, on, (==.), (>.) )

import           Local.Params           ( defaultCurrencyMonthlyParamining,
                                          defaultWalletCurrencies )
import           Local.Persist.Currency ( Currency )
import           Type.Money             ( Percent, mkPercent )
import           Type.Wallet            ( WalletData )

import           Database.Esqueleto
import           Text.Hamlet            ( shamletFile )
import           Yesod.WebSockets

import           Text.Pretty.Simple     ( pShowNoColor )


getSUNoticeParaminingUserListR :: Double -> Handler Html
getSUNoticeParaminingUserListR limit = do
    let amountLimit = defaultLimit
    (haveMore, uwDataPage) <- runDB $ getUsersWithParaminingDB 12 (toSqlKey 0)
    let items = uwDataPage
        render = renderer
    defaultLayout $(widgetFile "su/notice/paramining/wallet-list")

postSUNoticeParaminingUserListR :: Double -> Handler Html
postSUNoticeParaminingUserListR _ = do
    mayLimit <- runInputPost $ iopt doubleField "limit"
    let amountLimit = fromMaybe defaultLimit mayLimit
    (haveMore, uwDataPage) <- runDB $ getUsersWithParaminingDB 12 (toSqlKey 0)
    let items = uwDataPage
        render = renderer
    defaultLayout $(widgetFile "su/notice/paramining/wallet-list")


renderer :: (Entity User, Entity UserWallet, Currency, Percent) -> Widget
renderer (Entity uid u, Entity wid w, c, p) = do
    renderAmount <- handlerToWidget getAmountRenderer
    toWidget $(shamletFile "templates/su/notice/paramining/wallet-item.hamlet")


getUserParamingList :: Double -> Int -> UserWalletId -> Handler [(Entity User, WalletData)]
getUserParamingList amountLimit perPage walletOffset = do
    error "wip"
    -- webSocket $ suParaminingUserListSocket

suParaminingUserListSocket :: WebSocketsT Handler ()
suParaminingUserListSocket = do
    error "wip"
--     channels <- appChannels <$> getYesod
--     let bcDepositConfirm = appChannelsOperatorDepositConfirm channels
--     let bcWithdrawalRequest = appChannelsOperatorWithdrawalCreate channels
--     dcReadChan <- liftIO . atomically $ dupTChan bcDepositConfirm
--     wrReadChan <- liftIO . atomically $ dupTChan bcWithdrawalRequest
--     forever $ race_
--         (do
--             dc <- liftIO . atomically $ readTChan dcReadChan
--             send' $ typedUpdateJson "Deposit User Confirmation" dc)
--         (do
--             wr <- liftIO . atomically $ readTChan wrReadChan
--             send' $ typedUpdateJson "Withdrawal User Request" wr)
--   where
--     send' = sendTextData . decodeUtf8 . A.encode


-- | Optimized query for fetching page of users which have possible paramining
-- ready to be accrued.
-- Get all ('User', 'UserWallet') pairs if wallet have
-- paramining feature (i.e. 'defaultCurrencyMonthlyParamining' returns
-- @Just@ value for wallet's currency).
-- Matches are ordered by (user id, wallet id) to speed up lookups by
-- database index lookup.
-- Result is a tuple of 'Bool' and matched entries list. Boolean values
-- indicates is there additional page of data or not.
getUsersWithParaminingDB ::
       MonadIO m
    => Int -- ^ limit
    -> UserWalletId
    -- ^ id offset to speed up database lookup.  This meant to be an id of
    -- last fetched user
    -> SqlPersistT m (Bool, [(Entity User, Entity UserWallet, Currency, Percent)])
getUsersWithParaminingDB lim ofs = do
    lst <- select . from $ \(w `InnerJoin` u) -> do
        on (w ^. UserWalletUserId ==. u ^. UserId)
        orderBy [ asc (w ^. UserWalletId), asc (u ^. UserIdent) ]
        where_ $
                (w ^. UserWalletCurrency `in_` valList currencies)
            &&. (w ^. UserWalletId >. val ofs)
        limit $ fromIntegral lim + 1
        return (u, w)
    let haveNextPage = if length lst > lim then True else False
        dataPage = take lim $ map marshal lst
    return (haveNextPage, dataPage)
  where
    marshal (user, wallet) =
        let currency = (userWalletCurrency . entityVal) wallet
            rate = defaultCurrencyMonthlyParamining currency
            paramining = fromMaybe (mkPercent 0) rate
        in (user, wallet, currency, paramining)

    currencies = map fst . filter (isJust . snd) . zip defaultWalletCurrencies $
        map defaultCurrencyMonthlyParamining defaultWalletCurrencies

defaultLimit :: Double
defaultLimit = 10.0

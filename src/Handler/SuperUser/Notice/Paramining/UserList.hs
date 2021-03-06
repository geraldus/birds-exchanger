{-# LANGUAGE OverloadedStrings #-}
module Handler.SuperUser.Notice.Paramining.UserList where

import           Import                 hiding ( Value, on, (==.), (>.) )

import           Local.Params           ( defaultCurrencyMonthlyParamining,
                                          defaultWalletCurrencies )
import           Local.Persist.Currency ( Currency, currencyCodeT' )
import           Type.Money             ( Percent, mkPercent )
import           Utils.Type

import           Database.Esqueleto
import           Text.Hamlet            ( shamletFile )


getSUNoticeParaminingUserListR :: Double -> Handler Html
getSUNoticeParaminingUserListR amountLimit = do
    (_, uwDataPage) <- runDB $ getUsersWithParaminingDB 12 (toSqlKey 0)
    let items = uwDataPage
        render = renderer
    defaultLayout $(widgetFile "su/notice/paramining/wallet-list")

postSUNoticeParaminingUserListR :: Double -> Handler Html
postSUNoticeParaminingUserListR _ = do
    mayLimit <- runInputPost $ iopt doubleField "limit"
    let amountLimit = fromMaybe defaultLimit mayLimit
    (_, uwDataPage) <- runDB $ getUsersWithParaminingDB 12 (toSqlKey 0)
    let items = uwDataPage
        render = renderer
    defaultLayout $(widgetFile "su/notice/paramining/wallet-list")


renderer :: (Ent User, Ent UserWallet, Currency, Percent) -> Widget
renderer (Entity uid u, Entity _ w, c, _) = do
    renderAmount <- handlerToWidget getAmountRenderer
    toWidget $(shamletFile "templates/su/notice/paramining/wallet-item.hamlet")


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
    -> SqlPersistT m (Bool, [(Ent User, Ent UserWallet, Currency, Percent)])
getUsersWithParaminingDB lim ofs = do
    lst <- select . from $ \(w `InnerJoin` u) -> do
        on (w ^. UserWalletUserId ==. u ^. UserId)
        orderBy [ asc (w ^. UserWalletId), asc (u ^. UserIdent) ]
        where_ $
                (w ^. UserWalletCurrency `in_` valList currencies)
            &&. (w ^. UserWalletId >. val ofs)
        limit $ fromIntegral lim + 1
        return (u, w)
    let haveNextPage = length lst > lim
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

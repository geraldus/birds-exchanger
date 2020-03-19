{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.API.Admin.Users.List where

import           Import

import           Local.Params                  ( defaultWalletCurrencies )
import           Local.Persist.UserRole
import           Yesod.Auth.Util.PasswordStore ( makePassword )


apiAdminUsersList :: Handler [ Entity User ]
apiAdminUsersList =
    requireAdminId >> (runDB $ selectList [] [ Asc UserIdent ])

apiAdminDoesUserExists :: Handler (Maybe (Entity User))
apiAdminDoesUserExists = do
    _ <- requireAdminId
    login <- runPostForm
    runDB . getBy $ UniqueUser login
  where
    runPostForm = runInputPost $ ireq textField "username"

apiAdminSafelyCreateUser ::
    Text -> UserRole -> Text -> Handler (Maybe (Entity User, Entity Email))
apiAdminSafelyCreateUser login role pass = do
    _ <- requireAdminId
    existing <- runDB . getBy $ UniqueUser login
    case existing of
        Just _ -> return Nothing
        Nothing -> do
            hashedPass <- liftIO $
                decodeUtf8 <$> makePassword (encodeUtf8 pass) 14
            let user = User login (Just hashedPass) role
            uid <- insert' $ user
            let email = Email login (Just uid) Nothing
            eid <- insert' $ email
            refToken <- appNonce128urlT
            let ref = Referrer uid refToken
            _ <- insert' $ ref
            _ <- mapM (getOrCreateWallet uid) defaultWalletCurrencies
            zltwToken <- appNonce128urlT
            now <- liftIO getCurrentTime
            let zltWallet = ZltWallet uid 0 0 zltwToken now
            _ <- insert' zltWallet
            return (Just (Entity uid user, Entity eid email))
  where
    insert' :: (PersistEntity a, PersistEntityBackend a ~ SqlBackend) =>
        a -> Handler (Key a)
    insert' = runDB . insert


apiAdminSafelyUpdateUser ::
    UserId -> Text -> UserRole -> Text -> Handler (Maybe (Entity User, Entity Email))
apiAdminSafelyUpdateUser uid login role pass = do
    _ <- requireAdminId
    existingTargetUser <- runDB $ get uid
    existingNewName <- runDB . getBy $ UniqueUser login
    case existingTargetUser of
        Nothing -> return Nothing
        Just oldUserData -> case existingNewName of
            Nothing -> do
                hashedPass <- liftIO $
                    decodeUtf8 <$> makePassword (encodeUtf8 pass) 14
                let passwordUpdate = if length pass > 0
                        then [ UserPassword =. Just hashedPass ]
                        else [ ]
                let params = 
                        [ UserIdent =. login
                        , UserRole =. role ] 
                        <> passwordUpdate
                newUserData <- updateGet uid params
                email <- updateUserEmail uid login
                -- todo seek and update Email
                return (Just (Entity uid newUserData, Entity eid email))
            Just _ -> return Nothing
  where 
    updateUserEmail :: UserId -> Text -> Handler (Entity Email)
    updateUserEmail uid login = do
        existing <- runDB $ selectFirst [ EmailUser ==. Just uid ]
        case existing of
            Nothing -> createEmail
            Just e  -> error "123"
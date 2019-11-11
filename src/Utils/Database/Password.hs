module Utils.Database.Password where

import           Import.NoFoundation as I hiding ( on, (==.), (||.) )

import           Database.Esqueleto


getCredsByEmail
    :: (MonadIO m)
    => Text
    -> SqlPersistT m [(Entity Email, Entity User)]
getCredsByEmail login =
    select $ from $
        \(e `InnerJoin` u) -> do
            on (just (u ^. UserId) ==. e ^. EmailUserId)
            where_ (e ^. EmailEmail ==. val login)
            return (e, u)

getCredsByToken
    :: (MonadIO m)
    => Text
    -> SqlPersistT m [(Entity Email, Entity User, Entity PasswordResetToken)]
getCredsByToken token =
    select $ from $
        \(prt `InnerJoin` e `InnerJoin` u) -> do
            on (just (u ^. UserId) ==. e ^. EmailUserId)
            on (e ^. EmailId ==. prt ^. PasswordResetTokenEmail)
            where_ (prt ^. PasswordResetTokenToken ==. val token)
            return (e, u, prt)

module Utils.Database.User (
    queryClientCredsMaybe
) where

import           Import.NoFoundation hiding ( on, (==.) )

import           Data.Maybe          ( listToMaybe )
import           Database.Esqueleto

queryClientCredsMaybe ::
    MonadIO m => UserId -> SqlPersistT m (Maybe (Entity User, Entity Email))
queryClientCredsMaybe uid =
    (fmap listToMaybe) <$> select . from $
        \(u `InnerJoin` e) -> do
            on (just (u ^. UserId) ==. e ^. EmailUserId)
            where_ (u ^. UserId ==. val uid)
            limit 1
            return (u, e)
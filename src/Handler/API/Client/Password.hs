{-# LANGUAGE OverloadedStrings #-}
module Handler.API.Client.Password where

import           Import

import           Database.Persist.Sql          ( SqlBackend )
import           Yesod.Auth.Util.PasswordStore ( makePassword )

apiUnsafeChangeUserPassword ::
       UserId
    -> Text
    -> ReaderT SqlBackend Handler ()
apiUnsafeChangeUserPassword uid passtr = do
    saltedPassword <- liftIO $ decodeUtf8 <$>
        makePassword (encodeUtf8 passtr) 14
    update uid [ UserPassword =. Just saltedPassword ]

data PasswordUpdateResult
    = PasswordUpdateSuccess
    | PasswordUpdateError [(Text, Text)]

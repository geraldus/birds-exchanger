{-# LANGUAGE OverloadedStrings #-}
module Handler.API.Auth where

import           Import

import           Local.Auth.Plugin             ( PrizmPluginAuthResult (..),
                                                 checkCredsDB )

import           Yesod.Auth                    ( Creds (..), setCreds )


postAPI_AuthAuthenticateNoTokenR :: Handler TypedContent
postAPI_AuthAuthenticateNoTokenR = do
    (username, password) <- runInputPost
        ((,) <$> ireq textField "username"
             <*> ireq textField "password")
    apiAuthAuthenticateNoTokenR username password

apiAuthAuthenticateNoTokenR :: Text -> Text -> Handler TypedContent
apiAuthAuthenticateNoTokenR username pass = do
    res <- runDB $ checkCredsDB username pass
    case res of
        AuthSuccess -> do
            setCreds False (Creds "prizm auth plugin" username [])
            selectRep . provideRep $ pure $ object
                [ "success" .= ("ok" :: Text)
                , "ident" .= username
                ]
        _ -> do
            selectRep . provideRep $ pure $ object
                [ "status" .= ("fail" :: Text)
                ]
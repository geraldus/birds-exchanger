{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
module Local.Auth.Plugin
    ( PrizmAuthPlugin
    , authPrizm
    , loginR ) where


import           Import.NoFoundation

import           Yesod.Auth          ( AuthHandler, AuthPlugin (..), AuthRoute,
                                       Creds (..), Route (..), YesodAuth,
                                       loginErrorMessageI, setCredsRedirect )
import qualified Yesod.Auth.Message  as Msg
import           Yesod.Form          ( ireq, runInputPost, textField )

import           Control.Applicative ( (<$>), (<*>) )
import           Data.Text           ( Text )


-- TODO: FIXME: проверять статус активации при входе


loginR :: AuthRoute
loginR = PluginR "prizm auth plugin" ["login"]


class ( YesodAuth site
      , YesodPersist site
      , BaseBackend (YesodPersistBackend site) ~ SqlBackend
      , PersistUniqueRead (YesodPersistBackend site) )
      => PrizmAuthPlugin site


authPrizm :: PrizmAuthPlugin m => AuthPlugin m
authPrizm =
  AuthPlugin "prizm auth plugin" dispatch loginWidget
  where
    dispatch "POST" ["login"] = postLoginR >>= sendResponse
    dispatch _ _              = notFound
    loginWidget toMaster = do
      request <- getRequest
      [whamlet|
        $newline never
        <h1>PRIZM-Обменник | Вход
        <form method="post" action="@{toMaster loginR}">
          $maybe t <- reqToken request
            <input type=hidden name=#{defaultCsrfParamName} value=#{t}>
          <table>
            <tr>
              <th>_{Msg.UserName}
              <td>
                 <input type="text" name="username" required>
            <tr>
              <th>_{Msg.Password}
              <td>
                 <input type="password" name="password" required>
            <tr>
              <td colspan="2">
                 <button type="submit" .btn .btn-success>_{Msg.LoginTitle}
        |]


postLoginR :: (PrizmAuthPlugin site)
           => AuthHandler site TypedContent
postLoginR = do
    (username, password) <- runInputPost
        ((,) Control.Applicative.<$> ireq textField "username"
             Control.Applicative.<*> ireq textField "password")
    authRes <- checkCreds username password
    if authRes == AuthSuccess
        then setCredsRedirect (Creds "prizm auth plugin" username [])
        else loginErrorMessageI LoginR
                (if authRes == InvalidAuthPair
                    then Msg.InvalidUsernamePass
                    else Msg.IdentifierNotFound username)


checkCreds :: (PrizmAuthPlugin site) => Text -> Text -> AuthHandler site AuthResult'
checkCreds username password = liftHandler . runDB $ do
    mayUser <- getBy $ UniqueUser username
    return $ case mayUser of
        Nothing   -> NoSuchUser
        Just user -> if userPassword (entityVal user) == Just password
            then AuthSuccess
            else InvalidAuthPair


data AuthResult'
    = AuthSuccess
    | NoSuchUser
    | InvalidAuthPair
    deriving Eq

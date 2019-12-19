{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
module Local.Auth.Plugin
    ( PrizmAuthPlugin
    , PrizmPluginAuthResult(..)
    , authPrizm
    , checkCredsDB
    , loginR ) where


import           Import.NoFoundation

import           Yesod.Auth                    ( AuthHandler, AuthPlugin (..),
                                                 AuthRoute, Creds (..),
                                                 Route (..), YesodAuth,
                                                 loginErrorMessageI,
                                                 setCredsRedirect )
import qualified Yesod.Auth.Message            as Msg
import           Yesod.Auth.Util.PasswordStore ( verifyPassword )
import           Yesod.Form                    ( ireq, runInputPost, textField )

import           Control.Applicative           ( (<$>), (<*>) )
import           Data.Text                     ( Text )


-- TODO: FIXME: проверять статус активации при входе

loginR :: AuthRoute
loginR = PluginR "prizm auth plugin" ["login"]

class ( YesodAuth site
      , RenderMessage site (SomeMessage site)
      , YesodPersist site
      , BaseBackend (YesodPersistBackend site) ~ SqlBackend
      , PersistUniqueRead (YesodPersistBackend site) )
      => PrizmAuthPlugin site


authPrizm ::
       (PrizmAuthPlugin site, YesodPersistBackend site ~ SqlBackend)
    => AuthPlugin site
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
                        <button type="submit" .btn .btn-success>
                            _{Msg.LoginTitle}
            |]


postLoginR ::
       (PrizmAuthPlugin site, YesodPersistBackend site ~ SqlBackend)
    => AuthHandler site TypedContent
postLoginR = do
    (username, password) <- runInputPost
        ((,) Control.Applicative.<$> ireq textField "username"
             Control.Applicative.<*> ireq textField "password")
    genericPostLoginR username password

genericPostLoginR ::
       (PrizmAuthPlugin site, YesodPersistBackend site ~ SqlBackend)
        => Text -> Text -> AuthHandler site TypedContent
genericPostLoginR username password = do
    authRes <- checkCreds username password
    let pluginCreds = (Creds "prizm auth plugin" username [])
    -- TODO: FIXME: Check if user have verified email address
    if authRes == AuthSuccess
        then setCredsRedirect pluginCreds
        else
            loginErrorMessageI
                LoginR
                (if authRes == InvalidAuthPair
                    then Msg.InvalidUsernamePass
                    else Msg.IdentifierNotFound username)

checkCreds ::
       (PrizmAuthPlugin site, YesodPersistBackend site ~ SqlBackend)
    => Text -> Text -> AuthHandler site PrizmPluginAuthResult
checkCreds username password = liftHandler . runDB $ checkCredsDB username password


checkCredsDB ::
       MonadIO m
    => Text -> Text -> SqlPersistT m PrizmPluginAuthResult
checkCredsDB username password  = do
    mayUser <- getBy $ UniqueUser username
    return $ case mayUser of
        -- @TODO @FIXME: Make this it possible to control if we should
        -- expose "no such user" message or hide this fact returning
        -- "wrong login / password combination" message
        Nothing   -> NoSuchUser
        Just user -> do
            let mayPassword = userPassword (entityVal user)
                bsPass = encodeUtf8 password
            case mayPassword of
                Nothing -> AuthRejectPasswordNotSet
                Just storedPassword
                    | take 6 storedPassword == "sha256"
                        -> if verifyPassword bsPass (encodeUtf8 storedPassword)
                            then AuthSuccess
                            else InvalidAuthPair
                    | storedPassword == password -> AuthSuccessNeedReset
                    | otherwise -> InvalidAuthPair

data PrizmPluginAuthResult
    = AuthSuccess
    | NoSuchUser
    | InvalidAuthPair
    | AuthSuccessNeedReset
    | AuthRejectNeedReset
    | AuthRejectPasswordNotSet
    deriving Eq

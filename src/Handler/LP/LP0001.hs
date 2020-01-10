{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Handler.LP.LP0001 where

import           Import

import           Local.Persist.UserRole ( UserRole (Client) )


postLPHandler0001R :: Handler Html
postLPHandler0001R = do
    form <- runInputPostResult $ (,,)
        <$> ireq textField  "stocks"
        <*> ireq intField   "amount"
        <*> ireq emailField "email"
    case form of
        FormMissing -> redirect HomeR
        FormFailure es -> do
            mapM_ (addMessage "form" . toHtml) es
            redirect HomeR
        FormSuccess (stocks, amount, email) -> do
            -- test if email already present
            user <- getCreateUserRecord email
            purchase <- createPurchase user stocks amount
            redirect ClientSettingsR
  where
    getCreateUserRecord email = do
        u <- runDB . getBy $ UniqueUser email
        case u of
            Nothing -> createUser email
            Just e  -> return e

    createUser email = do
        pwd <- appNonce128urlT
        vk  <- appNonce128urlT
        let r = User email (Just pwd) Client
        rid <- runDB $ do
            id' <- insert r
            let e = Email email (Just id') (Just vk)
            insert_ e
            return id'
        return (Entity rid r)

    createPurchase u s a = do
        mayStocks <- runDB $ error "get stocks and actives"
        case mayStocks of
            [] -> error "warn about error"
            (s, a) : _ -> error "check amount and go on"

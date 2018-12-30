{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Profile where

import           Import
import           Type.Auth.SuperUser            ( SuperUser(..) )
import           Local.Persist.Wallet
import           Local.Persist.Currency         ( currSign )

import           Data.Maybe                     ( fromJust )


getProfileR :: Handler Html
getProfileR = do
    userName     <- userNameF . snd <$> requireAuthPair
    (_, wallets) <- requireClientData
    let walletIds  = map entityKey wallets
        walletVals = map entityVal wallets
        findWallet :: UserWalletId -> Entity UserWallet
        findWallet wid =
            fromJust $ find (\(Entity uwid w) -> uwid == wid) wallets
        findWalletCurrency = userWalletCurrency . entityVal . findWallet
    -- Let JS Front-end take and visualize data
    walletOps <- runDB $ selectList
        [WalletBalanceTransactionWalletId <-. walletIds]
        [Desc WalletBalanceTransactionTime, Desc WalletBalanceTransactionId]
    defaultLayout $ do
        setTitle . toHtml $ "Мой портфель | " <> userName
        $(widgetFile "profile")
  where
    userNameF (Left  u) = userIdent u
    userNameF (Right u) = suName u

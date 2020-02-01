{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.SignUpVerification where

import           Import
import           Local.Params           ( defaultWalletCurrencies )
import           Local.Persist.Currency ( Currency )


getSignUpVerifyR :: Text -> Text -> Handler Html
getSignUpVerifyR email verkey = do
    result <- runDB $ do
        mayEmail <- getBy $ UniqueEmail email
        case mayEmail of
            Just (Entity emailId emailRec)
                | emailVerkey emailRec == Just verkey -> do
                        update emailId $ [EmailVerkey =. Nothing]
                        -- _ <- createWallets ee
                        return Verified
                | isNothing (emailVerkey emailRec) -> return VerifiedAlready
                | otherwise -> return InvalidKey
            Nothing -> return InvalidEmail
    defaultLayout $ case result of
        Verified -> do
            addMessageI "email-validation" MsgMessageInfoEmailValidated
            redirect HomeR
        VerifiedAlready -> do
            addMessageI
                "email-validation" MsgMessageInfoEmailValidatedAlready
            redirect HomeR
        _ -> $(widgetFile "auth/invalid-verification-data")


data VerificationResult
    = Verified
    | VerifiedAlready
    | InvalidKey
    | InvalidEmail


createWallets :: forall (m :: * -> *) backend.
    ( MonadHandler m
    , PersistStoreWrite backend
    , HandlerSite m ~ App
    , BaseBackend backend ~ SqlBackend)
    => Entity Email -> ReaderT backend m [Entity UserWallet]
createWallets (Entity _ Email{..}) = do
    userId <- case emailUserId of
        Nothing  -> notFound
        Just uid -> return uid
    muser <- get userId
    case muser of
        Nothing -> notFound
        Just _ -> do
            idents <- mapM tokenize defaultWalletCurrencies
            time <- liftIO getCurrentTime
            let wallets = map (\(c, i) -> UserWallet userId c 0 i time Nothing) idents
            ids <- mapM insert wallets
            return $ zipWith Entity ids wallets
    where
        tokenize :: (MonadHandler m, HandlerSite m ~ App)
                 => Currency -> ReaderT backend m (Currency, Text)
        tokenize c = (,) c <$> liftHandler appNonce128urlT

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
module Handler.SignUpVerification where


import           Import
import Local.Persist.Currency


getSignUpVerifyR :: Text -> Text -> Handler Html
getSignUpVerifyR email verkey = do
    result <- runDB $ do
        mayEmail <- getBy $ UniqueEmail email
        case mayEmail of
            Just ee@(Entity emailId emailRec) ->
                if emailVerkey emailRec == Just verkey
                    then do
                        update emailId $ [EmailVerkey =. Nothing]
                        createWallets ee
                        return Verified
                    else if isNothing (emailVerkey emailRec)
                        then return VerifiedAlready
                        else return InvalidKey
            Nothing -> return InvalidEmail
    defaultLayout $ case result of
        Verified -> do
            setTitle "Проверка почты успешно пройдена!"
            redirect HomeR
        VerifiedAlready -> do
            setTitle "Проверка уже пройдена"
            redirect HomeR
        _ -> $(widgetFile "auth/invalid-verification-data")


postSignUpVerifyR :: Text -> Text -> Handler Html
postSignUpVerifyR email verkey =
    -- TODO: FIXME: Предоставить возможность повторно отправить письмо для активации
    return mempty


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
createWallets (Entity emailId Email{..}) = do
    userId <- case emailUserId of
        Nothing -> notFound
        Just uid -> return uid
    muser <- get userId
    case muser of
        Nothing -> notFound
        Just user -> do
            rurid <- liftHandler appNonce128urlT
            pzmid <- liftHandler appNonce128urlT
            time <- liftIO getCurrentTime
            let wallets =
                    [ UserWallet userId (FiatC RUR) 0 rurid time
                    , UserWallet userId (CryptoC PZM) 0 pzmid time
                    ]
            ids <- mapM insert wallets
            return $ zipWith Entity ids wallets
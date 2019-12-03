{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( getApplicationDev
    , appMain
    , develMain
    , makeFoundation
    , makeLogWare
    -- * for DevelMain
    , getApplicationRepl
    , shutdownApp
    -- * for GHCI
    , handler
    , db
    ) where

import           Import                               hiding ( isNothing, on,
                                                        update, (=.), (==.) )
import           Local.Persist.Exchange               ( ExchangePair (..) )
import           Market.Functions                     ( reduceDomStats )
import           Type.App
import           Utils.Database.Orders                ( selectActiveOrdersOf )

import           Control.Monad.Logger                 ( liftLoc, runLoggingT )
import qualified Crypto.Nonce                         as CN
import           Database.Esqueleto
import           Database.Persist.Postgresql          ( createPostgresqlPool,
                                                        pgConnStr, pgPoolSize,
                                                        runSqlPool )
import           Language.Haskell.TH.Syntax           ( qLocation )
import           Network.HTTP.Client.TLS              ( getGlobalManager )
import           Network.Wai                          ( Middleware )
import           Network.Wai.Handler.Warp             ( Settings,
                                                        defaultSettings,
                                                        defaultShouldDisplayException,
                                                        getPort, runSettings,
                                                        setHost, setOnException,
                                                        setPort )
import           Network.Wai.Middleware.RequestLogger ( Destination (Logger),
                                                        IPAddrSource (..),
                                                        OutputFormat (..),
                                                        destination,
                                                        mkRequestLogger,
                                                        outputFormat )
import           System.Log.FastLogger                ( defaultBufSize,
                                                        newStdoutLoggerSet,
                                                        toLogStr )


-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import           Handler.Admin.LogIn
import           Handler.API.App
import           Handler.API.Auth
import           Handler.API.News
import           Handler.BlackList
import           Handler.Client.Deposit
import           Handler.Client.HandleDeposit
import           Handler.Client.HandleWithdrawal
import           Handler.Client.Order.Create
import           Handler.Client.Orders
import           Handler.Client.Settings
import           Handler.Client.Withdrawal
import           Handler.Common
import           Handler.Home
import           Handler.Info
import           Handler.Manage.Info.Add
import           Handler.Manage.Info.Index
import           Handler.Manage.Info.Update
import           Handler.Operator.DepositRequestsList
import           Handler.Operator.HandleDeposit
import           Handler.Operator.HandleWithdrawal
import           Handler.Operator.LogIn
import           Handler.Operator.User.History
import           Handler.Operator.WebSocket
import           Handler.Operator.WithdrawalRequest
import           Handler.Profile
import           Handler.SignUp                       hiding ( from )
import           Handler.SignUpVerification
import           Handler.SuperUser.FinancialReport
import           Handler.SuperUser.WebSocket
import           Handler.TermsOfUse


-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
    -- Some basic initializations: HTTP connection manager, logger, and static
    -- subsite.
    appHttpManager <- getGlobalManager
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    appStatic <-
        (if appMutableStatic appSettings then staticDevel else static)
        (appStaticDir appSettings)
    appNonceGen <- liftIO CN.new
    chClientNotifications <- newBroadcastTChanIO
    chOperatorNotifications <- newBroadcastTChanIO
    chDepositUserConfirm <- newBroadcastTChanIO
    chWithdrawalRequest <- newBroadcastTChanIO
    appPaymentMethods <- newTMVarIO hardcodedPaymentMethods
    let appChannels = AppChannels
            { appChannelsClientNotifications = chClientNotifications
            , appChannelsOperatorNotifications = chOperatorNotifications
            , appChannelsOperatorDepositConfirm = chDepositUserConfirm
            , appChannelsOperatorWithdrawalCreate = chWithdrawalRequest
            }
    appOperatorsOnline <- newTMVarIO []

    -- We need a log function to create a connection pool. We need a connection
    -- pool to create our foundation. And we need our foundation to get a
    -- logging function. To get out of this loop, we initially create a
    -- temporary foundation without a real connection pool, get a log function
    -- from there, and then create the real foundation.
    let mkFoundation appConnPool appDOM = App {..}
        -- The App {..} syntax is an example of record wild cards. For more
        -- information, see:
        -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html
        tempFoundation = mkFoundation
            (error "connPool forced in tempFoundation")
            (error "fake DOM stats")
        logFunc = messageLoggerSource tempFoundation appLogger

    -- Create the database connection pool
    pool <- flip runLoggingT logFunc $ createPostgresqlPool
        (pgConnStr  $ appDatabaseConf appSettings)
        (pgPoolSize $ appDatabaseConf appSettings)

    -- Perform database migration using our application's logging settings.
    runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc

    -- Perform manual migraions
    let genToken = CN.nonce128urlT appNonceGen
    flip runSqlPool pool $ do
        migrateHelper_RefTokens genToken
        migrateHelper_GRef

    orders <- flip runSqlPool pool $ mapM
        selectActiveOrdersOf
            [ ExchangePzmRur, ExchangeOurRur, ExchangeOurPzm ]
    let statsDOM = reduceDomStats [] $ concat orders

    domTVar <- newTMVarIO statsDOM

    -- Return the foundation
    return $ mkFoundation pool domTVar

-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applying some additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- makeLogWare foundation
    -- Create the WAI application and apply middlewares
    appPlain <- toWaiAppPlain foundation
    return $ logWare $ defaultMiddlewaresNoLogging appPlain

makeLogWare :: App -> IO Middleware
makeLogWare foundation =
    mkRequestLogger def
        { outputFormat =
            if appDetailedRequestLogging $ appSettings foundation
                then Detailed True
                else Apache
                        (if appIpFromHeader $ appSettings foundation
                            then FromFallback
                            else FromSocket)
        , destination = Logger $ loggerSet $ appLogger foundation
        }


-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
      setPort (appPort $ appSettings foundation)
    $ setHost (appHost $ appSettings foundation)
    $ setOnException (\_req e ->
        when (defaultShouldDisplayException e) $ messageLoggerSource
            foundation
            (appLogger foundation)
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e))
      defaultSettings

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app <- makeApplication foundation
    return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    -- Get the settings from all relevant sources
    settings <- loadYamlSettingsArgs
        -- fall back to compile-time values, set to [] to require values at runtime
        [configSettingsYmlValue]

        -- allow environment variables to override
        useEnv

    -- Generate the foundation from the settings
    foundation <- makeFoundation settings

    -- Generate a WAI Application from the foundation
    app <- makeApplication foundation

    -- Run the application with Warp
    runSettings (warpSettings foundation) app


--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app1 <- makeApplication foundation
    return (getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()


---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h

-- | Run DB queries
db :: ReaderT SqlBackend Handler a -> IO a
db = handler . runDB


migrateHelper_RefTokens :: (MonadIO m) => IO Text -> SqlPersistT m ()
migrateHelper_RefTokens genToken = do
    noRefs <- select . from $ \ u -> do
        where_ . notExists . from $ \r ->
            where_ (r ^. ReferrerUser ==. u ^. UserId)
        return u
    mapM_ makeReferrer noRefs
  where
    makeReferrer (Entity u _) = liftIO genToken >>= insert_ . Referrer u

migrateHelper_GRef :: (MonadIO m) => SqlPersistT m ()
migrateHelper_GRef = do
    gUser <- select . from $ \(u `InnerJoin` ref) -> do
        on (u ^. UserId ==. ref ^. ReferrerUser)
        where_ (u ^. UserIdent ==. val great)
        return (u, ref)
    case gUser of
        [] -> return ()
        (_, Entity masterRef _) : _ -> do
            noRels <- select . from $ \ u -> do
                where_ (
                    (notExists . from $ \r ->
                        where_ (r ^. ReferralUser ==. u ^. UserId))
                    &&. not_ (u ^. UserIdent ==. val great)
                    )
                return u
            mapM_ (addReferral masterRef) (map entityKey noRels)
  where
    addReferral master client = insert_ (Referral client master)
    great = "heraldhoi@gmail.com"






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

import           Import                                        hiding ( groupBy,
                                                                 isNothing, on,
                                                                 update, (=.),
                                                                 (==.) )
import           Local.Persist.Currency                        ( Currency,
                                                                 ouroC, rubC )
import           Local.Persist.Exchange                        ( ExchangePair (..) )
import           Local.Persist.ReferralBounty                  ( ReferralBountyType (RegistrationBounty) )
import           Local.Persist.TransferMethod                  ( TransferMethod,
                                                                 ctmOuro,
                                                                 ftmAlphaRub,
                                                                 ftmPayPalRub,
                                                                 ftmQiwiRub,
                                                                 ftmSberRub,
                                                                 ftmTinkoffRub )
import           Market.Functions                              ( reduceDomStats )
import           Type.App
import           Utils.Database.Orders                         ( selectActiveOrdersOf )

import           Control.Monad                                 ( void )
import           Control.Monad.Logger                          ( liftLoc,
                                                                 runLoggingT )
import qualified Crypto.Nonce                                  as CN
import           Database.Esqueleto
import           Database.Persist.Postgresql                   ( createPostgresqlPool,
                                                                 pgConnStr,
                                                                 pgPoolSize,
                                                                 runSqlPool )
import           Language.Haskell.TH.Syntax                    ( qLocation )
import           Network.HTTP.Client.TLS                       ( getGlobalManager )
import           Network.Wai                                   ( Middleware )
import           Network.Wai.Handler.Warp                      ( Settings,
                                                                 defaultSettings,
                                                                 defaultShouldDisplayException,
                                                                 getPort,
                                                                 runSettings,
                                                                 setHost,
                                                                 setOnException,
                                                                 setPort )
import           Network.Wai.Middleware.RequestLogger          ( Destination (Logger),
                                                                 IPAddrSource (..),
                                                                 OutputFormat (..),
                                                                 destination,
                                                                 mkRequestLogger,
                                                                 outputFormat )
import           System.Log.FastLogger                         ( defaultBufSize, newStdoutLoggerSet,
                                                                 toLogStr )

import           Text.Pretty.Simple                            ( pPrint )

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import           Handler.Admin
import           Handler.APIs
import           Handler.Client
import           Handler.Client.Deposit
import           Handler.Client.HandleDeposit
import           Handler.Client.HandleWithdrawal
import           Handler.Client.Withdrawal
import           Handler.Common
import           Handler.Home
import           Handler.Info
import           Handler.Articles
import           Handler.LP.LP0001
import           Handler.Manage.Info.Add
import           Handler.Manage.Info.Index
import           Handler.Manage.Info.Update
import           Handler.Editor
import           Handler.Operator.DepositRequestsList
import           Handler.Operator.HandleDeposit
import           Handler.Operator.HandleWithdrawal
import           Handler.Operator.LogIn
import           Handler.Operator.Stocks.Purchase.Confirmation
import           Handler.Operator.Stocks.Purchase.Details
import           Handler.Operator.Stocks.Purchase.Index
import           Handler.Operator.User.History
import           Handler.Operator.WebSocket
import           Handler.Operator.WithdrawalRequest
import           Handler.Profile
import           Handler.SignUp
import           Handler.SignUpVerification
import           Handler.Stocks
import           Handler.SuperUser.FinancialReport
import           Handler.SuperUser.Notice
import           Handler.SuperUser.WebSocket
import           Handler.TermsOfUse
import           Handler.WebSocket.Client
import           Handler.WebSocket.Public


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
    appLogger      <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    appStatic <-
        (if appMutableStatic appSettings then staticDevel else static)
        (appStaticDir appSettings)
    appNonceGen             <- liftIO CN.new
    chClientNotifications   <- newBroadcastTChanIO
    chOperatorNotifications <- newBroadcastTChanIO
    chDepositUserConfirm    <- newBroadcastTChanIO
    chWithdrawalRequest     <- newBroadcastTChanIO
    chPublicNotifications   <- newBroadcastTChanIO
    appPaymentMethods       <- newTMVarIO hardcodedPaymentMethods
    let appChannels = AppChannels
            { appChannelsPublicNotifications      = chPublicNotifications
            , appChannelsClientNotifications      = chClientNotifications
            , appChannelsOperatorNotifications    = chOperatorNotifications
            , appChannelsOperatorDepositConfirm   = chDepositUserConfirm
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
    flip runSqlPool pool $ do
        migrateHelper_Stocks
        -- migrateHelper_GRef

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


migrateHelper_Stocks :: (MonadIO m) => SqlPersistT m ()
migrateHelper_Stocks = do
    let stocksStartMax = 10000
        stocksStartPriceCents = 100000
        stocksStartAbbr = "FNXB"
        stocksStartName = "FENIX START"
    let stocksStandardMax = 1000
        stocksStandardPriceCents = 1000000
        stocksStandardAbbr = "FNXS"
        stocksStandardName = "FENIX STANDARD"
    let stocksPremiumMax = 100
        stocksPremiumPriceCents = 5000000
        stocksPremiumAbbr = "FNXP"
        stocksPremiumName = "FENIX PREMIUM"
    mkStockActives
        stocksStartMax
        stocksStartPriceCents
        stocksStartAbbr
        stocksStartName
    mkStockActives
        stocksStandardMax
        stocksStandardPriceCents
        stocksStandardAbbr
        stocksStandardName
    mkStockActives
        stocksPremiumMax
        stocksPremiumPriceCents
        stocksPremiumAbbr
        stocksPremiumName
    return ()
  where
    mkStockActives ::
        MonadIO m => Int -> Int -> Text -> Text -> SqlPersistT m (Entity Stocks, Entity StocksActive)
    mkStockActives qnt cents abr name = do
        let stockActive sid = StocksActive sid qnt qnt
        rows <- select . from $ \(s `LeftOuterJoin` a) -> do
            on (just (s ^. StocksId) ==. a ?. StocksActiveStock)
            where_ (s ^. StocksAbbr ==. val abr)
            return (s, a)
        let s = Stocks name abr cents
        case rows of
            [] -> do
                sid <- insert s
                let a = stockActive sid
                aid <- insert a
                return (Entity sid s, Entity aid a)
            (s', Nothing) : _ -> do
                let a = stockActive (entityKey s')
                aid <- insert a
                return (s', Entity aid a)
            (s', Just a'):_ -> return (s', a')

migrateHelper_PrizmBounties :: MonadIO m => IO Text -> Int -> SqlPersistT m ()
migrateHelper_PrizmBounties genToken maxLevel = do
    purchases <- select . from $
        \(p `InnerJoin` u) -> do
            on (u ^. UserId ==. p ^. StocksPurchaseUser)
            let levelZeroBounty = from $ \b -> where_
                    ( (b ^. ReferralBountyLevel      ==. val 0
                    &&. (b ^. ReferralBountyReferrer ==. u ^. UserId)
                    &&. (b ^. ReferralBountyType     ==. val RegistrationBounty)) )
            groupBy
                ( p ^. StocksPurchaseUser
                , p ^. StocksPurchaseId
                , p ^. StocksPurchaseAccepted )
            where_
                ( not_ (isNothing (p ^. StocksPurchaseAccepted))
                &&. (isNothing (p ^. StocksPurchaseCancelled))
                &&. (notExists $ levelZeroBounty) )
            orderBy [ asc (p ^. StocksPurchaseAccepted)  ]
            return p
    let firstPurchases = takeFirstPurchasesOnly purchases
    mapM (accrueReferralBounties genToken maxLevel) firstPurchases
    return ()
  where
    takeFirstPurchasesOnly :: [Entity StocksPurchase] -> [Entity StocksPurchase]
    takeFirstPurchasesOnly ps = snd $ foldr step ([], []) ps
       where step pe@(Entity _ p) acc@(usrs, ps)  =
                if stocksPurchaseUser p `elem` usrs
                then acc
                else (stocksPurchaseUser p : usrs, pe : ps)

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
            mapM_ (addReferral' masterRef) (map entityKey noRels)
  where
    addReferral' master client = insert_ (Referral client master)
    great = "heraldhoi@gmail.com"

migrateHelper_UpdateCurrencies :: MonadIO m => SqlPersistT m ()
migrateHelper_UpdateCurrencies = void $ do
    wallets
    withdrawals
    deposits
    acceptedDeposits
    innerProfit
  where
        wallets :: MonadIO m => SqlPersistT m ()
        wallets = mapApplyPersistVals updateWalletCurrencies currencyUpdatePairs

        withdrawals :: MonadIO m => SqlPersistT m ()
        withdrawals = mapApplyPersistVals
            updateWithdrawalCurrencies transferMethodUpdatePairs

        deposits :: MonadIO m => SqlPersistT m ()
        deposits = do
            mapApplyPersistVals depositCurrencies currencyUpdatePairs
            mapApplyPersistVals depositTargetCurrencies currencyUpdatePairs
            mapApplyPersistVals depositTransferMethods transferMethodUpdatePairs

        acceptedDeposits :: MonadIO m => SqlPersistT m ()
        acceptedDeposits = do
            mapApplyPersistVals acceptedDepositCurrencies currencyUpdatePairs
            mapApplyPersistVals
                acceptedDepositTargetCurrencies currencyUpdatePairs

        innerProfit :: MonadIO m => SqlPersistT m ()
        innerProfit = mapApplyPersistVals
            innerProfitCurrencies currencyUpdatePairs

        currencyUpdatePairs :: [(Currency, Text)]
        currencyUpdatePairs = [(ouroC, "CryptoC OUR"), (rubC, "% RUR")]

        transferMethodUpdatePairs :: [(TransferMethod, Text)]
        transferMethodUpdatePairs =
            [ (ftmSberRub,    "%SberBankCard2CardFTM RUR")
            , (ftmAlphaRub,   "%AlphaBankCard2CardFTM RUR")
            , (ftmTinkoffRub, "%TinkoffBankCard2CardFTM RUR")
            , (ftmPayPalRub,  "%PayPalTransferFTM RUR")
            , (ftmQiwiRub,    "%QiwiFTM RUR")
            , (ctmOuro,       "CryptoTM OUR")
            ]


        updateWalletCurrencies ::
            MonadIO m => [PersistValue] -> SqlPersistT m ()
        updateWalletCurrencies = rawExecute qUpdateWalletCurrencies

        updateWithdrawalCurrencies ::
            MonadIO m => [PersistValue] -> SqlPersistT m ()
        updateWithdrawalCurrencies = rawExecute qUpdateWithdrawalCurrencies

        depositCurrencies :: MonadIO m => [PersistValue] -> SqlPersistT m ()
        depositCurrencies = rawExecute qUpdateDepositCurrencies

        depositTargetCurrencies ::
            MonadIO m => [PersistValue] -> SqlPersistT m ()
        depositTargetCurrencies = rawExecute qUpdateDepositTargetCurrencies

        depositTransferMethods ::
            MonadIO m => [PersistValue] -> SqlPersistT m ()
        depositTransferMethods = rawExecute qUpdateDepositTransferMethods

        acceptedDepositCurrencies ::
            MonadIO m => [PersistValue] -> SqlPersistT m ()
        acceptedDepositCurrencies =
            rawExecute qUpdateAcceptedDepositCurrencies

        acceptedDepositTargetCurrencies ::
            MonadIO m => [PersistValue] -> SqlPersistT m ()
        acceptedDepositTargetCurrencies =
            rawExecute qUpdateAcceptedDepositTargetCurrencies

        innerProfitCurrencies :: MonadIO m => [PersistValue] -> SqlPersistT m ()
        innerProfitCurrencies = rawExecute qUpdateInnerProfitRecordCurrencies

        -- dropUniqueUserWallets :: MonadIO m => SqlPersistT m ()
        -- dropUniqueUserWallets = rawExecute qDropUniqueUserWalletContraints []

        qUpdateWalletCurrencies =
            "update user_wallet set currency=? where currency like ?"

        qUpdateWithdrawalCurrencies =
            "update withdrawal_request set method=? where method like ?"

        qUpdateDepositCurrencies =
            "update deposit_request set currency=? where currency like ?"

        qUpdateDepositTargetCurrencies =
            "update deposit_request set target_currency=? where target_currency like ?"

        qUpdateDepositTransferMethods =
            "update deposit_request set transfer_method =? where transfer_method like ?"

        qUpdateAcceptedDepositCurrencies =
            "update accepted_deposit set currency=? where currency like ?"

        qUpdateAcceptedDepositTargetCurrencies =
            "update accepted_deposit set target_currency=? where target_currency like ?"

        qUpdateInnerProfitRecordCurrencies =
            "update inner_profit_record set currency=? where currency like ?"


        mapApplyPersistVals ::
               (MonadIO m, PersistField a, Show a, PersistField b)
            => ([PersistValue] -> SqlPersistT m ())
            -> [(a, b)]
            -> SqlPersistT m ()
        mapApplyPersistVals f = mapM_ $ \(a, b) ->
            f [ toPersistValue (show a), toPersistValue b ]


        -- qDropUniqueUserWalletContraints =
        --     "alter table user_wallet drop constraint unique_wallet"


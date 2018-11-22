{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
module Foundation where

import           Import.NoFoundation
import           Yesod.Auth.Hardcoded
import           Yesod.Auth.Message
import           Yesod.Core.Types         ( Logger )
import qualified Yesod.Core.Unsafe        as Unsafe
import           Yesod.Default.Util       ( addStaticContentExternal )

import           Control.Monad.Logger     ( LogSource )
import qualified Data.CaseInsensitive     as CI
import qualified Data.Text.Encoding       as TE
import           Data.Text.Format.Numbers ( PrettyCfg (..), prettyF )
import           Database.Persist.Sql     ( ConnectionPool, runSqlPool )
import           Text.Hamlet              ( hamletFile )
import           Text.Jasmine             ( minifym )

-- Extra imports
import           Local.Auth
import           Local.Persist.Currency
import           Local.Persist.UserRole
import           Type.Fee
import           Utils.Deposit            ( oneCoinCents )


import qualified Crypto.Nonce             as CN

import           Text.Read                ( readMaybe )


exchangerName :: Text
exchangerName = "Обменник PRIZM"

exchangerHost :: Text
exchangerHost = "http://development-host"

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    , appNonceGen    :: CN.Generator
    }

data MenuItem = MenuItem
    { menuItemLabel          :: Text
    , menuItemRoute          :: Route App
    , menuItemAccessCallback :: Bool
    }

data MenuTypes
    = NavbarLeft MenuItem
    | NavbarRight MenuItem

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a = forall (m :: * -> *).
    (MonadIO m) => ReaderT SqlBackend m a

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot :: Approot App
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing   -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
    yesodMiddleware = defaultYesodMiddleware

    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage
        muser <- maybeAuthPair :: Handler (Maybe (Either UserId Text, Either User SuperUser))
        let muserName = userNameF . snd <$> muser
        let isClient = isClientUser muser
        wallets <- if isClient then getUserWallets else pure []
        mcurrentRoute <- getCurrentRoute

        -- Get the breadcrumbs, as defined in the YesodBreadcrumbs instance.
        (title, parents) <- breadcrumbs

        -- Define the menu items of the header.
        let menuItems =
                [ NavbarLeft $ MenuItem
                    { menuItemLabel = "Главная"
                    , menuItemRoute = HomeR
                    , menuItemAccessCallback = True
                    }
                , NavbarLeft $ MenuItem
                    { menuItemLabel = "Портфель"
                    , menuItemRoute = ProfileR
                    , menuItemAccessCallback = isClientUser muser
                    }
                , NavbarLeft $ MenuItem
                    { menuItemLabel = "Пополни счёт"
                    , menuItemRoute = DepositR
                    , menuItemAccessCallback = isClientUser muser }
                , NavbarLeft $ MenuItem
                    { menuItemLabel = "Входящие заявки"
                    , menuItemRoute = OperatorDepositRequestsListR
                    , menuItemAccessCallback = isStaffUser muser }
                , NavbarRight $ MenuItem
                    { menuItemLabel = "Регистрация"
                    , menuItemRoute = SignUpR
                    , menuItemAccessCallback = isNothing muser
                    }
                , NavbarRight $ MenuItem
                    { menuItemLabel = "Вход"
                    , menuItemRoute = AuthR LoginR
                    , menuItemAccessCallback = isNothing muser
                    }
                , NavbarRight $ MenuItem
                    { menuItemLabel = "Выход"
                    , menuItemRoute = AuthR LogoutR
                    , menuItemAccessCallback = isJust muser
                    }
                ]

        let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
        let navbarRightMenuItems = [x | NavbarRight x <- menuItems]

        let navbarLeftFilteredMenuItems = [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
        let navbarRightFilteredMenuItems = [x | x <- navbarRightMenuItems, menuItemAccessCallback x]

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")
        where
            isClientUser (Just (_, Left u)) = userRole u == Client
            isClientUser _                  = False
            isStaffUser (Just (_, Left u))  = userRole u /= Client
            isStaffUser (Just (_, Right _)) = True
            isStaffUser _                   = False

    -- The page to be redirected to when authentication is required.
    authRoute
        :: App
        -> Maybe (Route App)
    authRoute _ = Just $ AuthR LoginR

    isAuthorized
        :: Route App  -- ^ The route the user is visiting.
        -> Bool       -- ^ Whether or not this is a "write" request.
        -> Handler AuthResult
    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _                        = return Authorized
    isAuthorized CommentR _                         = return Authorized
    isAuthorized HomeR _                            = return Authorized
    isAuthorized FaviconR _                         = return Authorized
    isAuthorized RobotsR _                          = return Authorized
    isAuthorized (StaticR _) _                      = return Authorized
    isAuthorized SignUpR _                          = return Authorized
    isAuthorized (SignUpVerifyR _ _) _              = return Authorized
    isAuthorized OperatorBidsR _                    = isStaffAuthenticated
    isAuthorized OperatorDepositRequestsListR _     = isStaffAuthenticated
    isAuthorized OperatorAcceptDepositRequestR _    = isStaffAuthenticated
    isAuthorized DepositR _                         = isClientAuthenticated
    isAuthorized (DepositRequestConfirmationR _) _  = isClientAuthenticated
    isAuthorized DepositConfirmRequestR _           = isClientAuthenticated
    isAuthorized ExchangeOrderCreateR _             = isClientAuthenticated

    -- the profile route requires that the user is authenticated, so we
    -- delegate to that function
    isAuthorized ProfileR _                        = isAuthenticated

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent
        :: Text  -- ^ The file extension
        -> Text -- ^ The MIME content type
        -> LByteString -- ^ The contents of the file
        -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger :: App -> IO Logger
    makeLogger = return . appLogger

-- Define breadcrumbs.
instance YesodBreadcrumbs App where
    -- Takes the route that the user is currently on, and returns a tuple
    -- of the 'Text' that you want the label to display, and a previous
    -- breadcrumb route.
    breadcrumb
        :: Route App  -- ^ The route the user is visiting currently.
        -> Handler (Text, Maybe (Route App))
    breadcrumb HomeR       = return ("Главная", Nothing)
    breadcrumb (AuthR _)   = return ("Вход", Just HomeR)
    breadcrumb ProfileR    = return ("Портфель", Just HomeR)
    breadcrumb DepositR    = return ("Внесение средств", Just ProfileR)
    breadcrumb WithdrawalR = return ("Внесение средств", Just ProfileR)
    breadcrumb  _          = return ("*", Nothing)

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

instance PrizmAuthPlugin App

instance YesodAuth App where
    type AuthId App = Either UserId Text

    -- Where to send a user after successful login
    loginDest :: App -> Route App
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest :: App -> Route App
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer :: App -> Bool
    redirectToReferer _ = True

    authenticate :: (MonadHandler m, HandlerSite m ~ App)
                 => Creds App -> m (AuthenticationResult App)
    authenticate Creds{..} = case credsPlugin of
        "hardcoded" -> return $ case lookupUser credsIdent of
            Nothing -> UserError InvalidLogin
            Just m  -> Authenticated $ Right $ suName m
        "prizm auth plugin" -> do
            x <- liftHandler $ runDB $ getBy $ UniqueUser credsIdent
            return $ case x of
                Just (Entity uid _) -> Authenticated $ Left uid
                Nothing             -> UserError InvalidLogin
        -- TODO: FIXME: Better error when plugin name not recognized
        _ -> return $ UserError InvalidLogin

    -- You can add other plugins like Google Email, email or OAuth here
    authPlugins :: App -> [AuthPlugin App]
    authPlugins _ = [authHardcoded, authPrizm]

-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    return $ case muid of
        Nothing -> Unauthorized "Войдите в систему для просмотра это страницы"
        Just _  -> Authorized

isClientAuthenticated :: Handler AuthResult
isClientAuthenticated = do
    ma <- maybeAuthPair
    return $ case ma of
        Nothing -> Unauthorized "Войдите в систему для просмотра этой страницы"
        Just (_, Right _) -> Unauthorized "Аккаунт оператора не имеет счёта"
        Just (_, Left _) -> Authorized

isStaffAuthenticated :: Handler AuthResult
isStaffAuthenticated = do
    ma <- maybeAuthPair
    return $ case ma of
        Nothing -> Unauthorized "Войдите в систему для просмотра это страницы"
        Just (_, Left user) -> case userRole user of
            Client ->
                Unauthorized "Для просмотра страницы нужен другой тип аккаунта"
            Operator -> Authorized
            Admin    -> Authorized
        Just (_, Right _) -> Authorized


instance YesodAuthPersist App where
    type AuthEntity App = Either User SuperUser

    getAuthEntity (Left uid) =
        do x <- liftHandler $ runDB (get uid)
           return (Left <$> x)
    getAuthEntity (Right username) = return (Right <$> lookupUser username)


instance YesodAuthHardcoded App where
    validatePassword u = return . validPassword u
    doesUserNameExist  = return . isJust . lookupUser

validPassword :: Text -> Text -> Bool
validPassword u p =
    case find (\m -> suName m == u && suPassword m == p) superUsers of
        Just _ -> True
        _      -> False


-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage :: App -> [Lang] -> FormMessage -> Text
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding


instance PathPiece (Either UserId Text) where
    fromPathPiece = readMaybe . unpack
    toPathPiece = pack . show


lookupUser :: Text -> Maybe SuperUser
lookupUser username = find (\m -> suName m == username) superUsers


appNonce128urlT :: Handler Text
appNonce128urlT = do
    g <- appNonceGen <$> getYesod
    liftIO $ CN.nonce128urlT g


requireClientId :: Handler UserId
requireClientId = do
    ap <- requireAuthPair
    case ap of
        (Right _  , _     ) -> redirect HomeR
        (Left  uid, Left u) -> case userRole u of
            Client -> return uid
            _ ->
                permissionDenied accessErrorClientOnly
        _ -> permissionDenied accessErrorClientOnly

maybeClient :: Handler (Maybe (Entity User, [Entity UserWallet]))
maybeClient = do
    mauth <- maybeAuthPair
    case mauth of
        Nothing -> return Nothing
        Just (Right _, _) -> return Nothing
        Just (Left uid, Left user) -> case userRole user of
            Client -> do
                wallets <- mapM (getOrCreateWallet uid) defaultWalletCurrencies
                return $ Just ((Entity uid user), wallets)
            _ -> return Nothing
        _ -> return Nothing

requireClient :: Handler (Entity User, [Entity UserWallet])
requireClient = do
    mclient <- maybeClient
    case mclient of
        Nothing         -> permissionDenied accessErrorClientOnly
        Just clientData -> return clientData

requireStaffId :: Handler (Either UserId Text)
requireStaffId = do
    mayAuth <- maybeAuthPair
    case mayAuth of
        -- SuperUsers authorized everywhere
        Just (uid, Right _  ) -> return uid
        -- For DB users: clients are rejected, operators and admins are allowed
        Just (uid, Left user) -> case userRole user of
            Client   -> notFound
            Operator -> return uid
            Admin    -> return uid
        _ -> notFound


userNameF :: Either User SuperUser -> Text
userNameF (Left  u) = userIdent u
userNameF (Right u) = suName u


getUserWallets :: Handler [(Int, Currency)]
getUserWallets = do
    mAuthPair <- maybeAuthPair
    case mAuthPair of
        Just (Left uid, Left _) -> do
            wallets <- runDB $ selectList [UserWalletUserId ==. uid] []
            return $
                map (\(Entity _ w) -> (userWalletAmountCents w, userWalletCurrency w)) wallets
        _ -> return []


headerUserBalanceRender :: [(Int, Currency)] -> Widget
headerUserBalanceRender [] = mempty
headerUserBalanceRender ((amtCents, cur):ws) = do
    let amt = truncate $ fromIntegral amtCents / fromIntegral oneCoinCents
    -- TODO: FIXME: Write functions to convert Int cents back to
    -- double and render function
    let cents = amtCents - amt * oneCoinCents
    let currencySign = case cur of
            FiatC RUR   -> "₽"
            CryptoC PZM -> "PZM"
            _           -> show cur
    [whamlet|
        <li .nav-item.mx-2>
            <span .navbar-text>
                <span .wallet-amount-value.font-weight-bold.text-warning>#{amt}.#{cents}
                <span .wallet-currency-signature>#{currencySign}
        |]
    headerUserBalanceRender ws


currencySelect :: Field (HandlerFor App) Currency
currencySelect = selectFieldList currencyOptions

currencyOptions :: [(Text, Currency)]
currencyOptions =
    [ ("₽ российский рубль", FiatC RUR)
    , ("PZM криптовалюта Prizm", CryptoC PZM) ]


accessErrorClientOnly :: Text
accessErrorClientOnly = "Доступно только для аккаунтов уровня \"Клиент\""


defaultWalletCurrencies :: [Currency]
defaultWalletCurrencies = [ FiatC RUR, CryptoC PZM ]


getOrCreateWalletDB :: UserId -> Currency -> SqlPersistT Handler (Entity UserWallet)
getOrCreateWalletDB userId currency = do
    walletTextId <- liftHandler $ appNonce128urlT
    time <- liftIO getCurrentTime
    let newWallet = UserWallet userId currency 0 walletTextId time
    eitherWallet <- insertBy newWallet
    return $ case eitherWallet of
        Left entity -> entity
        Right wid   -> Entity wid newWallet


getOrCreateWallet :: UserId -> Currency -> Handler (Entity UserWallet)
getOrCreateWallet uid = runDB . getOrCreateWalletDB uid


-- TODO: FIXME: Make an MVar or something and expand as app property.
-- Allow possibility to change this value on the fly
defaultExchangeFee :: Fee
defaultExchangeFee = Percent 1


currSign :: Currency -> Text
currSign (FiatC RUR)   = "₽"
currSign (CryptoC PZM) = "PZM"


dblCfg :: PrettyCfg
dblCfg = PrettyCfg 2 (Just '\'') '.'

cents2dblT :: Int -> Text
cents2dblT n = prettyF dblCfg (fromIntegral n / 100)

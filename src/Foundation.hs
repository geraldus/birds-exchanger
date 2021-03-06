{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Foundation where

import           Import.NoFoundation
import           Yesod.Auth.Hardcoded
import           Yesod.Auth.Message
import           Yesod.Core.Types                ( Logger )
import qualified Yesod.Core.Unsafe               as Unsafe
import           Yesod.Default.Util              ( addStaticContentExternal )
import           Yesod.Form.I18n.Russian

import           Local.Auth
import           Local.Params                    ( defaultWalletCurrencies )
import           Local.Persist.Currency
import           Local.Persist.TransferMethod
import           Local.Persist.UserRole
import           Market.Type                     ( DOMStats )
import           Paths_prizm_exchange            ( version )
import           Settings.MailRu                 ( projectSupportEmailCreds )
import           Type.App
import           Type.Money                      ( Percent (..) )
import           Type.Stocks                     ( ClientStocksData )
import           Type.Wallet                     ( WalletData (..) )
import           Utils.Common
import           Utils.Database.Password         ( getCredsByEmail )
import           Utils.Database.User             ( queryClientCredsMaybe )
import           Utils.Database.User.Stocks      ( queryClientStocks )
import           Utils.Database.User.Wallet      ( currencyAmountPara,
                                                   getOrCreateWalletDB,
                                                   getUserWalletStatsDB,
                                                   getUserWallets )
import           Utils.Form                      ( currencyOptionListRaw,
                                                   transferOptionsRaw )
import           Utils.Money                     ( fixedDoubleT )
import           Utils.Render                    ( renderCurrencyAmount )

import           Control.Monad.Logger            ( LogSource )
import           Control.Monad.Trans.Writer.Lazy ( Writer )
import qualified Crypto.Nonce                    as CN
import           Data.Aeson                      as A
import qualified Data.CaseInsensitive            as CI
import           Data.List                       ( findIndex )
import           Data.Semigroup                  ( Endo )
import qualified Data.Text.Encoding              as TE
import qualified Data.Text.Encoding.Error        as TEE
import           Data.Time.Clock.POSIX           ( utcTimeToPOSIXSeconds )
import           Data.Version                    ( showVersion )
import           Database.Persist.Sql            ( ConnectionPool, fromSqlKey,
                                                   runSqlPool )
import qualified Network.Wai                     as W
import           Text.Cassius                    ( cassiusFile )
import           Text.Hamlet                     ( hamletFile )
import           Text.Jasmine                    ( minifym )
import           Text.Julius                     ( rawJS )
import           Text.Read                       ( readMaybe )


data AppChannels = AppChannels
    { appChannelsPublicNotifications      :: TChan Value
    , appChannelsClientNotifications      :: TChan (UserId, Value)
    , appChannelsOperatorNotifications    :: TChan Value
    , appChannelsOperatorDepositConfirm   :: TChan (Entity DepositRequest)
    , appChannelsOperatorWithdrawalCreate :: TChan (Entity WithdrawalRequest) }

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings        :: AppSettings
    , appStatic          :: Static -- ^ Settings for static file serving.
    , appConnPool        :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager     :: Manager
    , appLogger          :: Logger
    , appNonceGen        :: CN.Generator
    , appChannels        :: AppChannels
    , appPaymentMethods  :: TMVar AppPaymentMethods
    , appOperatorsOnline :: TMVar [ Text ]
    , appDOM             :: TMVar DOMStats
    }

-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes.yesodroutes")

mkMessage "App" "messages" "ru"

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
        fromMaybe
            (getApprootText guessApproot app req) (appRoot $ appSettings app)

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    maximumContentLengthIO _ _ = pure $ Just $ 25 * 1024 * 1024

    -- Yesod Middleware allows you to run code before and after each
    -- handler function.
    -- The defaultYesodMiddleware adds the response header
    -- "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in
    --      either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware:
    -- yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module
    -- of the yesod-core package.
    yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
    yesodMiddleware = defaultYesodMiddleware

    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        let appVersion = showVersion version
        master          <- getYesod
        muser           <- maybeAuthPair
        currentRoute    <- getCurrentRoute
        sessionMessages <- getMessages
        mr              <- getMessageRender
        renderedUrl     <- getUrlRender
        let settings           = appSettings master
        let projType           = appType settings
        let muserName          = userNameF . snd <$> muser
        let isClientLoggedIn   = isClientUser muser
        let isStaffLoggedIn    = isStaffUser muser
        let isEditorLoggedIn   = isEditorUser muser
        let isAdminLoggedIn    = isAdminUser muser
        let isOperatorLoggedIn = isOperatorUser muser
        let isSuLoggedIn       = isSU muser
        let maybeClientUser = join $ eitherClientToMaybe <$> muser
        (wallets, stocks) <- if isClientLoggedIn
            then getUserBalances
            else pure ([ ], [ ])
        case currentRoute of
            Just (PasswordResetR _) -> return ()
            Just url
                | url `notElem`  [ AuthR LogoutR, AuthR LoginR, SignUpR, ClientRequestEmailVerificationR ] ->
                        setUltDest url
                | url == AuthR LogoutR -> setUltDest HomeR
                | otherwise -> do
                    rm <- W.requestMethod . reqWaiRequest <$> getRequest
                    if rm == methodPost
                        then setUltDest HomeR
                        else return ()
            _ -> return ()
        -- Get the breadcrumbs, as defined in the YesodBreadcrumbs instance.
        (title, parents) <- breadcrumbs
        -- Define the menu items of the header.
        let stocksItems = if projType /= FenixApp
                then [ ]
                else
                    [ NavbarLeft $ MenuItem
                        { menuItemLabel = mr MsgStocks
                        , menuItemRoute = StocksR
                        , menuItemAccessCallback = True } ]
        let menuItems =
                stocksItems
                <>
                [ NavbarLeft $ MenuItem
                    { menuItemLabel = mr MsgMenuTitleArticles
                    , menuItemRoute = ArticlesListR
                    , menuItemAccessCallback = True }
                ]
                <>
                [ NavbarLeft $ MenuItem
                    { menuItemLabel = mr MsgMenuTitleNews
                    , menuItemRoute = InfoListR
                    , menuItemAccessCallback = True }
                ]
                <>
                [ NavbarLeft $ MenuItem
                    { menuItemLabel = mr MsgMenuTitleRialto
                    , menuItemRoute = RialtoR
                    , menuItemAccessCallback = True }
                ]
                <>
                [ NavbarRight $ MenuItem
                    { menuItemLabel = mr MsgMenuTitleSignUp
                    , menuItemRoute = SignUpR
                    , menuItemAccessCallback = isNothing muser
                    }
                , NavbarRight $ MenuItem
                    { menuItemLabel = mr MsgMenuTitleSignIn
                    , menuItemRoute = AuthR LoginR
                    , menuItemAccessCallback = isNothing muser
                    }
                ]
        let appClientItems =
                [ MenuItem
                    { menuItemLabel = mr MsgClientProfilePageTitle
                    , menuItemRoute = ProfileR
                    , menuItemAccessCallback = isClientLoggedIn
                    }
                , MenuItem
                    { menuItemLabel = mr MsgClientOrdersPageTitle
                    , menuItemRoute = ClientOrdersR
                    , menuItemAccessCallback = isClientLoggedIn
                    }
                , MenuItem
                    { menuItemLabel = mr MsgDeposit
                    , menuItemRoute = DepositR
                    , menuItemAccessCallback = isClientLoggedIn }
                , MenuItem
                    { menuItemLabel = mr MsgWithdraw
                    , menuItemRoute = WithdrawalR
                    , menuItemAccessCallback = isClientLoggedIn }
                ]

        let clientMenuItems =
                appClientItems
                <>
                [ MenuItem
                    { menuItemLabel = mr MsgClientProfileSettingsPageTitle
                    , menuItemRoute = ClientSettingsR
                    , menuItemAccessCallback = isClientLoggedIn
                    } ]

        let operatorRequestsMenuItems =
                [ MenuItem
                    { menuItemLabel = mr MsgDeposit
                    , menuItemRoute = OperatorDepositRequestsListR
                    , menuItemAccessCallback = isOperatorLoggedIn }
                , MenuItem
                    { menuItemLabel = mr MsgWithdraw
                    , menuItemRoute = OperatorWithdrawalRequestsListR
                    , menuItemAccessCallback = isOperatorLoggedIn } ]
        let operatorStocksMenuItems =
                [ MenuItem
                    { menuItemLabel = mr MsgNavLabelStocksPurchase
                    , menuItemRoute = OperatorStocksPurchaseIndexR
                    , menuItemAccessCallback = isOperatorLoggedIn } ]

        let editorMenuItems =
                [ MenuItem
                    { menuItemLabel = mr MsgInfo
                    , menuItemRoute = ManageInfoIndexR
                    , menuItemAccessCallback = isEditorLoggedIn }
                , MenuItem
                    { menuItemLabel = mr MsgMenuTitleArticles
                    , menuItemRoute = EditorArticlesIndexR
                    , menuItemAccessCallback = isEditorLoggedIn } ]

        let adminMenuItems =
                [ MenuItem
                    { menuItemLabel = mr MsgMenuTitleAdminManageUsers
                    , menuItemRoute = AdminUsersListR
                    , menuItemAccessCallback = isAdminLoggedIn } ]

        let suMenuItems =
                [ MenuItem
                    { menuItemLabel = mr MsgFinancialReport
                    , menuItemRoute = SuperUserFinancialReportViewR
                    , menuItemAccessCallback = isSuLoggedIn } ]

        let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
        let navbarRightMenuItems = [x | NavbarRight x <- menuItems]

        let navbarLeftFilteredMenuItems =
                [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
        let navbarRightFilteredMenuItems =
                [x | x <- navbarRightMenuItems, menuItemAccessCallback x]

        navUserDropdownId   <- newIdent
        navManageDropdownId <- newIdent

        let projNavBGColor = if projType == FenixApp
                then "#0d011c" :: Text
                else "rgb(14, 14, 14)"
        let logoSrc = if projType == FenixApp
                then renderedUrl $ StaticR images_logo_060119_png
                else renderedUrl $ StaticR images_logo_header_png
        let (hostname, _)     = projectNameHost projType
        let (supportEmail, _) = projectSupportEmailCreds projType

        let jsonStocksPurchases = decodeUtf8 . encode $
                map clientStocksToJSON stocks

        let defaultMobileNav = $(widgetFile "default/nav/mobile")
        let defaultDesktopNav = $(widgetFile "default/nav/desktop")

        let jQueryScriptUrl = renderedUrl
                (StaticR _3rd_party_jquery_jquery_3_4_1_min_js)
        let jsCookieScriptUrl = renderedUrl
                (StaticR _3rd_party_js_cookie_js_cookie_2_2_1_min_js)
        let clientSocketUrl = renderedUrl ClientNotificationsWebSocketR

        setReferrerHttpOnlyCookie
        pc <- widgetToPageContent $ do
            case currentRoute of
                (Just (ArticleViewR a)) -> articleMetasToWidgetHead a
                _                       -> return ()
            addStylesheetAttrs
                (StaticR _3rd_party_fontawesome_css_all_min_css)
                [("defer", "defer")]
            addStylesheetAttrs
                (StaticR _3rd_party_bootstrap_css_bootstrap_min_css)
                [("async", "async")]
            addScriptAttrs
                (StaticR _3rd_party_bootstrap_js_bootstrap_bundle_min_js)
                [("defer", "defer")]
            toWidget $(cassiusFile "templates/default/wrapper.cassius")
            $(widgetFile "form/common")
            $(widgetFile "default/nav")
            -- $(widgetFile "maintainance-mode-message")
            $(widgetFile "default/layout")
        withUrlRenderer $(hamletFile "templates/default/wrapper.hamlet")
        where
            isStaffUser (Just (_, Left u))  = userRole u /= Client
            isStaffUser (Just (_, Right _)) = True
            isStaffUser _                   = False
            isClientUser = maybe
                False (either (hasUserRole Client) (const False) . snd)
            isEditorUser = maybe
                False (either (hasUserRole Editor) (const True) . snd)
            isOperatorUser = maybe
                False (either ((Operator ==) . userRole) (const True) . snd)
            isAdminUser = maybe
                False (either (hasUserRole Admin) (const True) . snd)
            isSU = maybe False (either (const False) (const True) . snd)

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
    isAuthorized (AuthR _) _                         = return Authorized
    isAuthorized FaviconR _                          = return Authorized
    isAuthorized RobotsR _                           = return Authorized
    isAuthorized HomeR _                             = return Authorized
    isAuthorized StocksR _                           = return Authorized
    isAuthorized RialtoR _                           = return Authorized
    isAuthorized (StaticR _) _                       = return Authorized
    isAuthorized SignUpR _                           = return Authorized
    isAuthorized (SignUpVerifyR _ _) _               = return Authorized
    isAuthorized PasswordChangeR _                   = return Authorized
    isAuthorized PasswordChangeGuideR _              = return Authorized
    isAuthorized (PasswordResetR _) _                = return Authorized
    isAuthorized PublicNotificationsWebSocketR _     = return Authorized
    -- CLIENT
    isAuthorized ClientNotificationsWebSocketR _     = isClientAuthenticated
    isAuthorized ClientRequestEmailVerificationR _   = return Authorized
    isAuthorized ProfileR _                          = isAuthenticated
    isAuthorized DepositR _                          = isClientAuthenticated
    isAuthorized WithdrawalR _                       = isClientAuthenticated
    isAuthorized (DepositRequestConfirmationR _) _   = isClientAuthenticated
    isAuthorized DepositConfirmRequestR _            = isClientAuthenticated
    isAuthorized ClientCancelDepositR _              = isClientAuthenticated
    isAuthorized WithdrawalCreateR True              = isClientAuthenticated
    isAuthorized ClientCancelWithdrawalR True        = isClientAuthenticated
    isAuthorized WithdrawalCreateR False             = postOnly
    isAuthorized ClientCancelWithdrawalR False       = postOnly
    isAuthorized (ClientStocksPurchaseR) _           = isClientAuthenticated
    isAuthorized ExchangeOrderCreateR True           = isClientAuthenticated
    isAuthorized ExchangeOrderCreateR False          = postOnly
    isAuthorized ClientOrdersR _                     = isClientAuthenticated
    isAuthorized (ClientOrderViewR _) _              = isClientAuthenticated
    isAuthorized ClientOrderCancelR _                = isClientAuthenticated
    isAuthorized (ClientStocksPurchaseDetailsR _) _  = isClientAuthenticated
    isAuthorized (ClientStocksPurchaseConfirmationR _) _ = isClientAuthenticated
    isAuthorized ClientSettingsR _                   = isClientAuthenticated
    -- STAFF
    isAuthorized AdminLogInR _                       = return Authorized
    -- OPERATORS
    isAuthorized OperatorLogInR _                    = return Authorized
    isAuthorized OperatorDepositRequestsListR _      = isStaffAuthenticated
    isAuthorized OperatorAcceptDepositRequestR _     = isStaffAuthenticated
    isAuthorized OperatorDeclineDepositRequestR _    = isStaffAuthenticated
    isAuthorized OperatorWithdrawalRequestsListR _   = isStaffAuthenticated
    isAuthorized OperatorAcceptWithdrawalRequestR _  = isStaffAuthenticated
    isAuthorized OperatorDeclineWithdrawalRequestR _ = isStaffAuthenticated
    isAuthorized OperatorStocksPurchaseIndexR _      = isOperatorAuthenticated
    isAuthorized OperatorStocksPurchaseArchiveR _    = isOperatorAuthenticated
    isAuthorized OperatorStocksPurchaseAcceptedR _   = isOperatorAuthenticated
    isAuthorized (OperatorStocksPurchaseDetailsR _) _ = isOperatorAuthenticated
    isAuthorized (OperatorStocksPurchaseConfirmationR _) _ = isOperatorAuthenticated
    isAuthorized (OperatorUserHistoryR _) _          = isOperatorAuthenticated
    isAuthorized OperatorWebSocketR _                = isOperatorAuthenticated
    -- EDITORS
    isAuthorized EditorArticlesIndexR _              = isEditorAuthenticated
    isAuthorized EditorArticleAddR _                 = isEditorAuthenticated
    isAuthorized (EditorArticleUpdateR _) _          = isEditorAuthenticated
    -- ADMINS
    isAuthorized ManageInfoIndexR _                  = isEditorAuthenticated
    isAuthorized ManageInfoAddR _                    = isEditorAuthenticated
    isAuthorized ManageInfoUpdateR _                 = isEditorAuthenticated
    isAuthorized AdminUsersListR _                   = isAdminAuthenticated
    isAuthorized AdminUsersCreateR _                 = isAdminAuthenticated
    isAuthorized (AdminUsersUpdateR _) _             = isAdminAuthenticated
    -- SUPER USERS
    isAuthorized SUNoticeIndexR _                    = isSuperUserAuthenticated
    isAuthorized (SUNoticeParaminingUserListR _) _   = isSuperUserAuthenticated
    isAuthorized SuperUserFinancialReportViewR _     = isSuperUserAuthenticated
    isAuthorized SuperUserWebSocketR _               = isSuperUserAuthenticated
    -- ALL: Common routes (guests including)
    isAuthorized ArticlesListR _                     = return Authorized
    isAuthorized (ArticleViewR _) _                  = return Authorized
    isAuthorized InfoListR _                         = return Authorized
    isAuthorized (InfoViewR _) _                     = return Authorized
    isAuthorized TermsOfUseR _                       = return Authorized
    -- API
    isAuthorized APIAppConfigR _                     = return Authorized
    isAuthorized APIAuthAuthenticateNoTokenR _       = return Authorized
    isAuthorized APIUserPasswordChangeR _            = return Authorized
    isAuthorized APIStocksAvailabilityR _            = return Authorized
    isAuthorized APINewsListR _                      = return Authorized
    isAuthorized APIArticleAddR _                    = isEditorAuthenticated
    isAuthorized (APIArticleUpdateR _) _             = isEditorAuthenticated
    isAuthorized APIStocksOperatorCancelPurchaseR _  =
        authorizeRoles [ Local.Persist.UserRole.SuperUser, Admin, Operator ]
    isAuthorized APIAdminUsersDoesUserExistsR _      = return Authorized
    isAuthorized LPHandler0001R _                    = return Authorized

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

    errorHandler (PermissionDenied msg) = do
        render <- getMessageRender
        let message = render MsgMessagePleaseLogIn
        selectRep $ do
            provideRep $ defaultLayout $ defaultMessageWidget
                (toHtml message)
                [hamlet|<p>#{msg}|]
            provideRep $
                return $ object ["message" .= (message <> ". " <> msg)]
            provideRep $ return $ message <> ". " <> msg
    errorHandler NotFound = do
        render <- getMessageRender
        let message = render MsgMessagePageNotFound
        selectRep $ do
            provideRep $ defaultLayout $ do
                r <- waiRequest
                let path' = TE.decodeUtf8With TEE.lenientDecode $
                        W.rawPathInfo r
                defaultMessageWidget (toHtml message) [hamlet|<p>#{path'}|]
            provideRep $ return $ object ["message" .= message]
            provideRep $ return message

    errorHandler other = defaultErrorHandler other

articleMetasToWidgetHead :: Text -> Widget
articleMetasToWidgetHead alias = do
    a <- liftHandler $ runDB $ getBy $ UniqueArticleAlias alias
    case a of
        Nothing -> return mempty
        Just (Entity _ article) -> toWidgetHead $ [shamlet|
            $maybe metaDesc <- articleMetaDesc article
                <meta name=description content="#{metaDesc}"/>
            $maybe metaKWords <- articleMetaKWords article
                <meta name=keywords content="#{metaKWords}"/>
            |]

-- Define breadcrumbs.
instance YesodBreadcrumbs App where
    -- Takes the route that the user is currently on, and returns a tuple
    -- of the 'Text' that you want the label to display, and a previous
    -- breadcrumb route.
    breadcrumb :: Route App  -- ^ The route the user is visiting currently.
        -> Handler (Text, Maybe (Route App))
    breadcrumb r = do
        mr <- getMessageRender
        isFenixApp <- (== FenixApp) . appType . appSettings <$> getYesod
        -- TODO: FIXME: Do not show breadcrumbs for non-staff users
        breadcrumb' isFenixApp mr r
      where
        breadcrumb' ::
               Bool
            -> (AppMessage -> Text)
            -> Route App  -- ^ The route the user is visiting currently.
            -> Handler (Text, Maybe (Route App))
        breadcrumb' isFenix mr HomeR =
            return (mr (projectName isFenix), Nothing)
        breadcrumb' _ _ (AuthR _)   = return ("Вход", Just HomeR)
        breadcrumb' _ _ SignUpR     = return ("Регистрация", Just HomeR)
        breadcrumb' _ mr StocksR    = return (mr MsgPageTitleStocks, Just HomeR)
        breadcrumb' _ mr ClientRequestEmailVerificationR =
            return (mr MsgPageTitleRequestEmailVerification, Just HomeR)
        breadcrumb' _ mr PasswordChangeR =
            return (mr MsgPasswordSetupTitle, Just HomeR)
        breadcrumb' _ mr PasswordChangeGuideR =
            return (mr MsgPasswordChangeGuideTitle, Just HomeR)
        breadcrumb' _ mr (PasswordResetR _) =
            return (mr MsgPasswordSetupTitle, Just HomeR)
        breadcrumb' _ _ ProfileR    = return ("Портфель", Just HomeR)
        breadcrumb' _ mr RialtoR    = return (mr MsgPageBreadcrumbsRialto, Just HomeR)
        breadcrumb' _ _ ClientOrdersR =
            return ("Мои ордера на обмен", Just HomeR)
        breadcrumb' _ mr ClientSettingsR =
            return (mr MsgClientProfileSettingsPageTitle, Just HomeR)
        breadcrumb' _ _ (ClientOrderViewR oid) = return
            ("Ордер #" <> (pack . show  .fromSqlKey) oid, Just ClientOrdersR)
        breadcrumb' _ _ DepositR    = return ("Внесение средств", Just ProfileR)
        breadcrumb' _ mr (ClientStocksPurchaseDetailsR _) =
            return (mr MsgBreadcrumbsTitleStocksDetails, Just StocksR)
        breadcrumb' _ _ (DepositRequestConfirmationR _) =
            return ("Подтверждение", Just DepositR)
        breadcrumb' _ _ WithdrawalR = return ("Вывод средств", Just ProfileR)
        breadcrumb' _ _ WithdrawalCreateR =
            return ("Вывод средств", Just ProfileR)
        breadcrumb' _ _ OperatorLogInR = return ("Оператор / Вход", Just HomeR)
        breadcrumb' _ _ OperatorDepositRequestsListR =
            return ("Заявки на пополнение", Just HomeR)
        breadcrumb' _ _ OperatorWithdrawalRequestsListR =
            return ("Заявки на вывод", Just HomeR)
        breadcrumb' _ mr OperatorStocksPurchaseIndexR =
            return (mr MsgBreadcrumbsTitleStocksIndex, Nothing)
        breadcrumb' _ mr OperatorStocksPurchaseArchiveR =
            return ( mr MsgBreadcrumbsTitleStocksArchive
                   , Just OperatorStocksPurchaseIndexR)
        breadcrumb' _ mr OperatorStocksPurchaseAcceptedR =
            return ( mr MsgBreadcrumbsTitleStocksAccepted
                   , Just OperatorStocksPurchaseIndexR)
        breadcrumb' _ mr (OperatorStocksPurchaseDetailsR _) =
            return ( mr MsgBreadcrumbsTitleStocksDetails
                   , Just OperatorStocksPurchaseIndexR )
        breadcrumb' _ mr EditorArticlesIndexR =
            return ( mr MsgBreadcrumbsTitleEditorArticles
                   , Just HomeR )
        breadcrumb' _ mr EditorArticleAddR =
            return ( mr MsgBreadcrumbsTitleEditorArticleAdd
                   , Just EditorArticlesIndexR )
        breadcrumb' _ mr (EditorArticleUpdateR _) =
            return ( mr MsgBreadcrumbsTitleEditorArticleUpdate
                   , Just EditorArticlesIndexR )
        breadcrumb' _ mr AdminUsersListR =
            return (mr MsgBreadcrumbsTitleAdminUsersIndex, Just HomeR)
        breadcrumb' _ mr AdminUsersCreateR = return
            (mr MsgBreadcrumbsTitleAdminUsersCreate, Just AdminUsersListR)
        breadcrumb' _ mr (AdminUsersUpdateR _) = return
            (mr MsgBreadcrumbsTitleAdminUsersUpdate, Just AdminUsersListR)
        breadcrumb' _ _ SuperUserFinancialReportViewR =
            return ("Финансовая отчётность", Just HomeR)
        breadcrumb' _ _ AdminLogInR =
            return ("Вход для супер-пользователя", Just HomeR)
        breadcrumb' _ mr TermsOfUseR = return (mr MsgTermsOfUse, Just HomeR)
        breadcrumb' _ mr InfoListR = return (mr MsgInfoListTitle, Just HomeR)
        breadcrumb' _ _ (InfoViewR alias) = do
            i <- runDB $ getBy404 (UniqueInfoAlias alias)
            return ((infoTitle . entityVal) i, Just InfoListR)
        breadcrumb' _ mr ArticlesListR =
            return (mr MsgBreadcrumbsTitleArticlesList, Just HomeR)
        breadcrumb' _ _ (ArticleViewR alias) = do
            i <- runDB $ getBy404 (UniqueArticleAlias alias)
            return ((articleTitle . entityVal) i, Just ArticlesListR)
        breadcrumb' _ mr ManageInfoIndexR =
                return (mr MsgManage <> " / " <> mr MsgInfo, Just HomeR)
        breadcrumb' _ mr ManageInfoAddR =
                return (mr MsgNewArticle, Just ManageInfoIndexR)
        breadcrumb' _ _ _          = return ("Ошибка", Just HomeR)
        projectName f = if f
            then MsgProjectNameFenix
            else MsgProjectNameOutbirds


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
    redirectToReferer _ = False

    authenticate :: (MonadHandler m, HandlerSite m ~ App)
                 => Creds App -> m (AuthenticationResult App)
    authenticate Creds{..} = case credsPlugin of
        "hardcoded" -> return $ case lookupUser credsIdent of
            Nothing -> UserError InvalidLogin
            Just m  -> Authenticated . Right $ suName m
        "prizm auth plugin" -> do
            -- TODO: FIXME: Check if email is verified
            x <- liftHandler . runDB . getBy $ UniqueUser credsIdent
            return $ case x of
                Just (Entity uid _) -> Authenticated $ Left uid
                Nothing             -> UserError InvalidLogin
        -- TODO: FIXME: Better error when plugin name not recognized
        _ -> return $ UserError InvalidLogin

    -- You can add other plugins like Google Email, email or OAuth here
    authPlugins :: App -> [AuthPlugin App]
    authPlugins _ = [authHardcoded, authPrizm]

    renderAuthMessage _ ("ru":_)   = russianMessage
    renderAuthMessage msg (_:rest) = renderAuthMessage msg rest
    renderAuthMessage _ []         = defaultMessage

    loginHandler :: AuthHandler App Html
    loginHandler = do
        ma <- maybeAuthId
        tp <- getRouteToParent
        when (isJust ma) (redirect HomeR)
        authLayout $ do
            setAppPageTitle MsgSignInPageTitle
            $(whamletFile "templates/auth/signin.hamlet")

postOnly :: Handler AuthResult
postOnly = do
    mr <- getMessageRender
    return $ Unauthorized (mr MsgWrongRequest)

-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    return $ case muid of
        Nothing -> Unauthorized "Войдите в систему для просмотра это страницы"
        Just _  -> Authorized

isClientAuthenticated :: Handler AuthResult
isClientAuthenticated = authorizeRoles [ Client ]

isStaffAuthenticated :: Handler AuthResult
isStaffAuthenticated = do
    ma <- maybeAuthPair
    case ma of
        Nothing -> notFound
        Just (Right _, _) -> return Authorized
        Just (_, Left u) -> if userRole u /= Client
            then return Authorized
            else notFound
        _ -> error "Impossible happened, dual staff and client authorization"

isAdminAuthenticated :: Handler AuthResult
isAdminAuthenticated = authorizeStaffRoles [ Admin ]

isEditorAuthenticated :: Handler AuthResult
isEditorAuthenticated = authorizeStaffRoles [ Editor ]

isOperatorAuthenticated :: Handler AuthResult
isOperatorAuthenticated = authorizeStaffRoles [ Operator ]

isSuperUserAuthenticated :: Handler AuthResult
isSuperUserAuthenticated = authorizeStaffRoles []

authorizeRoles :: [ UserRole ] -> Handler AuthResult
authorizeRoles rs = authorizeRolesRedirect rs Nothing Nothing

authorizeStaffRoles :: [ UserRole ] -> Handler AuthResult
authorizeStaffRoles rs =
    authorizeRolesRedirect rs (Just notFound) (Just notFound)


authorizeRolesRedirect ::
       [ UserRole ]
    -> Maybe (Handler AuthResult)
    -> Maybe (Handler AuthResult)
    -> Handler AuthResult
authorizeRolesRedirect rs maybeGuestHandler maybeOtherHandler = do
    ma <- maybeAuthPair
    case ma of
        Nothing ->
            fromMaybe (unauthorizedI MsgPleaseLogInText) maybeGuestHandler
        Just (_, Left user) -> if userRole user `elem` rs
            then pure Authorized
            else fromMaybe (unauthorizedI MsgPleaseLogInText) maybeOtherHandler
        Just (_, Right _) -> pure Authorized


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
    renderMessage _ ("ru":_)   = russianFormMessage
    renderMessage msg (_:rest) = renderMessage msg rest
    renderMessage _ []         = defaultFormMessage


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

instance PathPiece Double where
    fromPathPiece = readMaybe . unpack
    toPathPiece = pack . show


appNonce128urlT :: Handler Text
appNonce128urlT = do
    g <- appNonceGen <$> getYesod
    liftIO $ CN.nonce128urlT g


lookupUser :: Text -> Maybe SuperUser
lookupUser username = find (\m -> suName m == username) superUsers

requireClientId :: Handler UserId
requireClientId = do
    auth <- requireAuthPair
    case auth of
        (Right _  , _     ) -> redirect HomeR
        (Left  uid, Left u) -> case userRole u of
            Client -> return uid
            _ ->
                permissionDenied accessErrorClientOnly
        _ -> permissionDenied accessErrorClientOnly

requireClientCreds :: Handler (Entity Email, Entity User)
requireClientCreds = do
    auth <- requireAuthPair
    case auth of
        (Left _, Left (User ident _ Client)) -> do
            ret <- runDB $ getCredsByEmail ident
            case ret of
                val : _ -> return val
                []      -> permissionDenied accessErrorClientOnly
        _ -> permissionDenied accessErrorClientOnly

requireClientData :: Handler (Entity User, [Entity UserWallet])
requireClientData = do
    client <- maybeClient
    case client of
        Nothing         -> permissionDenied accessErrorClientOnly
        Just clientData -> return clientData

requireClientData' ::
    Handler ( Entity User
            , [Entity UserWallet]
            , [WalletData]
            , [(Entity StocksPurchase, Entity Stocks)] )
requireClientData' = do
    uid <- requireClientId
    runDB $ do
        user <- getJustEntity uid
        wallets <- getUserWallets uid
        wd <- mapM getUserWalletStatsDB wallets
        sd <- queryClientStocks uid
        return (user, wallets, wd, sd)

maybeClient :: Handler (Maybe (Entity User, [Entity UserWallet]))
maybeClient = do
    auth <- maybeAuthPair
    case auth of
        Nothing -> return Nothing
        Just (Right _, _) -> return Nothing
        Just (Left uid, Left user) -> case userRole user of
            Client -> do
                wallets <- mapM (getOrCreateWallet uid) defaultWalletCurrencies
                return $ Just (Entity uid user, wallets)
            _ -> return Nothing
        _ -> return Nothing

maybeClientId :: Handler (Maybe UserId)
maybeClientId = do
    p <- maybeAuthPair
    return . fmap fst . join $ eitherClientToMaybe <$> p

maybeClientAuthPair :: Handler (Maybe (UserId, User))
maybeClientAuthPair = do
    p <- maybeAuthPair
    return . join $ eitherClientToMaybe <$> p

maybeClientCreds :: Handler (Maybe (Entity User, Entity Email))
maybeClientCreds = do
    -- TODO Cache results
    auth <- maybeAuthId
    case auth of
        Just (Left uid) -> runDB $ queryClientCredsMaybe uid
        _               -> return Nothing

eitherClientToMaybe ::
    (Either UserId Text, Either User SuperUser) -> Maybe (UserId, User)
eitherClientToMaybe (Left uid, Left user@(User _ _ Client)) = Just (uid, user)
eitherClientToMaybe _                                       = Nothing

requireOperatorId :: Handler (Either UserId Text)
requireOperatorId = requireRolesId True [ Operator ] notFound

requireEditorId :: Handler (Either UserId Text)
requireEditorId = requireRolesId True [ Editor, Admin ] notFound

requireAdminId :: Handler (Either UserId Text)
requireAdminId = requireRolesId True [ Admin ] notFound

requireRolesId ::
       Bool
    -> [ UserRole ]
    -> Handler (Either UserId Text)
    -> Handler (Either UserId Text)
requireRolesId suIncluding roles action = do
    mayAuth <- maybeAuthPair
    case mayAuth of
        Just (suid, Right _) -> if suIncluding
            then return suid else action
        Just (ident, Left u) -> if userRole u `elem` roles
            then return ident else action
        _ -> action

requireSu :: Handler (Either UserId Text)
requireSu = do
    mayAuth <- maybeAuthPair
    case mayAuth of
        Just (uid, Right _  ) -> return uid
        _                     -> notFound

hasUserRole :: UserRole -> User -> Bool
hasUserRole r = (r ==) . userRole

hasUserRoles :: [ UserRole ] -> User -> Bool
hasUserRoles rs = flip elem rs . userRole


userNameF :: Either User SuperUser -> Text
userNameF (Left  u) = userIdent u
userNameF (Right u) = suName u

getUserBalances ::Handler ([WalletData], [ClientStocksData])
getUserBalances = do
    mAuthPair <- maybeAuthPair
    case mAuthPair of
        Just (Left uid, Left _) -> runDB $ do
            wallets <- getUserWallets uid
            wd <- mapM getUserWalletStatsDB wallets
            sd <- queryClientStocks uid
            return (wd, sd)
        _ -> return ([ ], [ ])


accessErrorClientOnly :: Text
accessErrorClientOnly = "Доступно только для аккаунтов уровня \"Клиент\""

getOrCreateWallet :: UserId -> Currency -> Handler (Entity UserWallet)
getOrCreateWallet u c = do
    token <- appNonce128urlT
    runDB $ getOrCreateWalletDB u token c

fsAddPlaceholder :: FieldSettings App -> Text -> FieldSettings App
fsAddPlaceholder settings p = let
        attrs = fsAttrs settings ++ [("placeholder", p)]
    in settings { fsAttrs = attrs }

fsAddClasses :: FieldSettings App -> [ Text ] -> FieldSettings App
fsAddClasses settings cs = let
        attrs = fsAttrs settings
        csi = unwords cs
        attrs' = updateAttrs attrs [] csi
    in settings { fsAttrs = attrs' }
    where
        updateAttrs [] acc classes = acc ++ [ ("class", classes) ]
        updateAttrs (a@(aname, aval):rest) acc classes
            | aname == "class" =
                    acc ++ [ ("class", aval <> " " <> classes) ] ++ rest
            | otherwise = updateAttrs rest (acc ++ [ a ]) classes

fsAddAttrs :: [ ( Text, Text) ] -> FieldSettings App -> FieldSettings App
fsAddAttrs attrs settings =
    let as = fsAttrs settings
    in settings { fsAttrs = as <> attrs }

fsBs4 :: FieldSettings App
fsBs4 = fsWithClasses [ "form-control" ] "" Nothing Nothing Nothing []

fsBs4WithId :: Text -> FieldSettings App
fsBs4WithId ident = fsWithClasses
    [ "form-control" ] "" Nothing (Just ident) Nothing []

fsWithClasses ::
       [ Text ]
    -> SomeMessage App
    -> Maybe (SomeMessage App)
    -> Maybe Text
    -> Maybe Text
    -> [ ( Text, Text ) ]
    -> FieldSettings App
fsWithClasses classList lbl tlt mid mnam attrs =
    let cs = unwords classList
        as = attrs <> [ ( "class", cs ) ]
    in FieldSettings lbl tlt mid mnam as

currencySelect :: Field (HandlerFor App) Currency
currencySelect = selectField . pure $ mkOptionList currencyOptionListRaw

transferMethodSelect :: Field (HandlerFor App) TransferMethod
transferMethodSelect = selectField . pure $ mkOptionList transferOptionsRaw

setAppTitle :: [ AppMessage ] -> Widget
setAppTitle ms = do
    projType <- appType . appSettings <$> handlerToWidget getYesod
    let (projName, extra) = if projType == FenixApp
            then (MsgProjectNameFenix, [ ])
            else ( MsgProjectNameOutbirds
                 , [MsgCurrencyExchangeService, MsgCurrencyExchangeSlang] )

    setCompositeTitle $
        ms <> [projName] <> extra

setAppPageTitle :: AppMessage -> Widget
setAppPageTitle = setAppTitle . (: [])

getNextPaymentAddressee ::
       (PaymentMethod -> (Maybe PaymentAddress, PaymentMethod))
    -> TransferMethod
    -> Handler (Maybe PaymentAddress)
getNextPaymentAddressee = getNextPaymentGeneric matchMethod

getNextPaymentGeneric ::
       (TransferMethod -> PaymentMethod -> Bool)
    -> (PaymentMethod -> (Maybe PaymentAddress, PaymentMethod))
    -> TransferMethod
    -> Handler (Maybe PaymentAddress)
getNextPaymentGeneric matchMethodFn selectNext targetTransferMethod = do
    pasMVar <- appPaymentMethods <$> getYesod
    liftIO . atomically $ do
        v <- takeTMVar pasMVar
        let nothingFound = putTMVar pasMVar v >> return Nothing
        let knownPaymentMethods = selectMethods targetTransferMethod v
        let mayIndex = findIndex
                (matchMethodFn targetTransferMethod) knownPaymentMethods
        case mayIndex of
            Nothing -> nothingFound
            Just i -> do
                let (prev, current:rest) = splitAt i knownPaymentMethods
                if noAddrs current
                    then nothingFound
                    else do
                        let (addr, updates) = selectNext current
                        case addr of
                            Nothing -> nothingFound
                            Just paymentAddr -> do
                                let nextState = prev <> [updates] <> rest
                                putTMVar
                                    pasMVar
                                    -- Make next state via Record update
                                    (updateMethods
                                        targetTransferMethod v nextState)
                                return (Just paymentAddr)
    where
        noAddrs :: PaymentMethod -> Bool
        noAddrs (FiatPaymentMethod _ _ [])   = True
        noAddrs (CryptoPaymentMethod _ _ []) = True
        noAddrs _                            = False

        selectMethods :: TransferMethod -> AppPaymentMethods -> [PaymentMethod]
        selectMethods (FiatTM _ _) app = appDepositFiatMethods app
        selectMethods (CryptoTM _) app = appDepositCryptoMethods app

        updateMethods
            :: TransferMethod
            -> AppPaymentMethods
            -> [PaymentMethod]
            -> AppPaymentMethods
        updateMethods (FiatTM _ _) app updates = app
                { appDepositFiatMethods = updates }
        updateMethods (CryptoTM _) app updates = app
                { appDepositCryptoMethods = updates }

matchMethod :: TransferMethod -> PaymentMethod -> Bool
matchMethod (FiatTM tm tc) (FiatPaymentMethod (FiatTM pm pc) _ _)
    = tm == pm && tc == pc
matchMethod (CryptoTM tc) (CryptoPaymentMethod pc _ _)
    = tc == pc
matchMethod _ _ = False

renderWalletBalanceW :: WalletData -> [Text] -> Widget
renderWalletBalanceW stats _listClasses = do
    let (Entity _ w) = walletDataWallet stats
        c = userWalletCurrency w
        a = userWalletAmountCents w
        o = walletDataOrdersCents stats
        r = walletDataWithdrawalCents stats
        cents = a + o + r
        paraminingRate = walletDataParaminingRate stats
        lastParaTime = walletDataLastParaTime stats
        paraVals = (,) <$> paraminingRate <*> lastParaTime
    amountRender <- handlerToWidget getAmountRenderer
    [whamlet|
        <span .ml-2 .navbar-text .wallet .balance>
            #{amountRender currencyClassNames amountClassNames False c a}
            <br>
            $maybe (r, t) <- paraVals
                ^{renderParaminingW t r cents c}
        |]
  where
    amountClassNames = ["font-weight-bold", "wallet-balance-val"]
    currencyClassNames = ["wallet-balance-currency"]

renderParaminingW :: UTCTime -> Percent -> Int -> Currency -> Widget
renderParaminingW t' _paraminingRate n c = do
    now <- liftIO getCurrentTime
    let paraCents = if n < 1
            then Just (0, 0, t')
            else (\(v, k) -> (v, k, t')) <$> currencyAmountPara now t' c n
    case paraCents of
        Nothing -> mempty
        Just (p, k, t) -> [whamlet|
            <small .wallet .paramining>
                <i .fas .fa-angle-double-up .stats-icon style="font-size: 0.65rem">
                <span .value .para .#{currencyCode} style="font-size: 0.9rem">
                    #{fixedDoubleT 7 (p / 100)}
                <small .currency .para .#{currencyCode}>
                    #{symbol}
            <script>
                window['para_#{currencyCode}'] = {
                    timestamp: #{fixedDoubleT 12 (realToFrac (utcTimeToPOSIXSeconds t) * 1000)},
                    amount: #{show n},
                    k_s: #{fixedDoubleT 12 (realToFrac k)}
                };
            |]
  where
    symbol = currencySymbol c

    currencyCode = (toLower . currencyCodeT') c


setReferrerHttpOnlyCookie :: Handler ()
setReferrerHttpOnlyCookie = do
    token <- referrerQueryString
    case token of
        Nothing -> return ()
        Just refToken -> do
            referrer <- runDB . getBy $ UniqueReferrerToken refToken
            case referrer of
                Nothing -> return ()
                Just _ -> do
                    name <- appRefTokenCookieName . appSettings <$> getYesod
                    addHeader "Set-Cookie" $
                        concat [ name, "=" , refToken , "; HttpOnly" ]

-- | Read referrer cookie.
referrerCookie :: Handler (Maybe Text)
referrerCookie = getYesod >>= lookupCookie . appRefTokenCookieName . appSettings

referrerQueryString :: Handler (Maybe Text)
referrerQueryString = do
    name <- appRefTokenParamName . appSettings <$> getYesod
    lookupGetParam name

getAmountRenderer ::
        Handler ([Text] -> [Text] -> Bool -> Currency -> Int -> Html)
getAmountRenderer = selectLocale >>= return . renderCurrencyAmount


twoColsLayout ::
    Html -> Maybe Text -> AppMessage -> Widget -> Widget -> Widget -> Widget
twoColsLayout _extraHtml maybeId pageTitle title leftCol rightCol = do
    htmlId <- maybe newIdent return maybeId
    setAppTitle [ pageTitle ]
    $(widgetFile "page/layout/two-cols-lg")


singleColLayout ::
    Html -> Maybe Text -> AppMessage -> Widget -> Widget -> Widget
singleColLayout _extraHtml maybeId pageTitle title content = do
    htmlId <- maybe newIdent return maybeId
    setAppTitle [ pageTitle ]
    $(widgetFile "page/layout/single-col")

redirectWithMessages ::
        [(Text, AppMessage)]
    -> [(Text, Html)]
    -> Route App
    -> Writer (Endo [ProvidedRep Handler]) ()
redirectWithMessages ms ts r = provideRep $ do
    mapM_ (uncurry addMessageI) ms
    mapM_ (uncurry addMessage)  ts
    (redirect r :: Handler Html)

respondJSONErrorMessages ::
    Text -> [(Text, Text)] -> Writer (Endo [ProvidedRep Handler]) ()
respondJSONErrorMessages m ts = provideRep . pure . object $
    [ "status" .= ("fail" :: Text)
    , "message" .= m
    ]
    <> if length ts > 0 then [ "errors" .= toJSON ts ] else [ ]

clientStocksToJSON :: (Entity StocksPurchase, Entity Stocks) -> Value
clientStocksToJSON (Entity _ p, Entity sid s)= object
    [ "pack-name"   .= stocksName s
    , "pack-id"     .= fromSqlKey sid
    , "pack-abbr"   .= stocksAbbr s
    , "pack-price"  .= stocksPrice s
    , "amount"      .= stocksPurchaseAmount p
    , "purchase-id" .= stocksPurchaseToken p
    , "time"        .= stocksPurchaseAccepted p ]

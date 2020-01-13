{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Client.Settings where

import           Handler.API.Client.Password   ( apiUnsafeChangeUserPassword )
import           Import
import           Settings.MailRu               ( password, serverName, smtpPort,
                                                 usernameOutbirdsNoreply )
import           Utils.Common                  ( projectSupportNameHost )
import           Utils.Database.Password       ( getCredsByEmail,
                                                 getCredsByToken )
import           Utils.Database.User.Referral  ( getCreateRefTokenDB,
                                                 getReferralsOfDB )
import           Utils.QQ                      ( stFile )

import qualified Data.Text.Lazy                as TL
import           Data.Time.Clock               ( addUTCTime )
import           Network.HaskellNet.SMTP
import           Network.HaskellNet.SMTP.SSL
import           Text.Blaze.Html.Renderer.Text ( renderHtml )
import           Text.Hamlet                   ( shamletFile )


getClientSettingsR :: Handler Html
getClientSettingsR = do
    (user, _) <- requireClientData
    token <- appNonce128urlT
    refToken <- runDB $ getCreateRefTokenDB (entityKey user) token
    urlRender <- getUrlRenderParams
    s <- appSettings <$> getYesod
    let refLink = urlRender
            HomeR
            [(appRefTokenParamName s, (referrerToken (entityVal refToken)))]
    defaultLayout $ do
        setAppPageTitle MsgClientSettingsPageTitle
        $(widgetFile "client/settings")

getPasswordChangeR :: Handler Html
getPasswordChangeR =
    defaultLayout $ do
        setAppPageTitle MsgPasswordSetupTitle
        $(widgetFile "auth/password-request-change")

postPasswordChangeGuideR :: Handler Html
postPasswordChangeGuideR = do
    login <- runInputPostResult $ ireq textField "username"
    case login of
        FormSuccess email -> do
            setUltDest PasswordChangeR
            res <- withExistingLogin email $ \existingLogin -> do
                token <- createPasswordChangeToken existingLogin
                sendPasswordChangeLink existingLogin token
                return passwordResetGuide
            defaultLayout $ do
                setAppPageTitle MsgPasswordChangeGuideTitle
                [whamlet|#{res}|]
        FormFailure errors -> do
            mapM_ (addMessageI "form") errors
            redirect PasswordChangeR
        FormMissing -> redirect PasswordChangeR
  where
    sendPasswordChangeLink ::
        Entity Email -> Entity PasswordResetToken -> Handler [(Text, Html)]
    sendPasswordChangeLink emailEntity tokenEntity = do
        messageRender <- getMessageRender
        urlRender <- getUrlRender
        let email = (emailEmail . entityVal) emailEntity
        let token = (passwordResetTokenToken . entityVal) tokenEntity
        let url = (urlRender . PasswordResetR) token
            homeUrl = urlRender HomeR
        projType <- appType . appSettings <$> getYesod
        liftIO $ do
            conn <- connectSMTPSSLWithSettings
                (unpack serverName)
                (defaultSettingsSMTPSSL { sslPort = smtpPort })
            authSuccess <-
                Network.HaskellNet.SMTP.SSL.authenticate
                    PLAIN
                    (unpack usernameOutbirdsNoreply)
                    (unpack password)
                    conn
            ret <- if authSuccess
                then do
                    let (from, _, exHost) = projectSupportNameHost projType
                        subject = messageRender
                            MsgMessageClientPasswordResetTitle
                    sendMimeMail (unpack email)
                                 (unpack from)
                                 (unpack subject)
                                 (textContent url exHost)
                                 (htmlContent url homeUrl email exHost)
                                 []
                                 conn
                    return []
                else do
                    let message = (toHtml . messageRender)
                            MsgAPIFailedToSendEmailAuthenticationFailed
                    return [("send-email", message)]
            closeSMTP conn
            return ret

    textContent :: Text -> Text -> TL.Text
    textContent linkUrl exchangerHost =  fromStrict
            [stFile|templates/mail/password-reset-link.text|]

    htmlContent :: Text -> Text -> Text -> Text -> TL.Text
    htmlContent linkUrl siteUrl userName exchangerHost = renderHtml
        $(shamletFile "templates/mail/password-reset-link.hamlet")

    passwordResetGuide :: Html
    passwordResetGuide =
            $(shamletFile "templates/auth/password-reset-guide.hamlet")

getPasswordResetR :: Text -> Handler Html
getPasswordResetR token = do
    set <- runDB $ getCredsByToken token
    now <- liftIO getCurrentTime
    defaultLayout $ do
        setAppPageTitle MsgPasswordSetupTitle
        case set of
            (Entity eid e, _, Entity _ t):_ -> do
                    let created = passwordResetTokenCreated t
                    let lt = fromIntegral $
                            passwordResetTokenLifeTimeMinutes t * 60
                    let expires = addUTCTime lt created
                    if now > expires
                        then do
                            handlerToWidget $ removeObsoleteTokens eid
                            addMessageI
                                "token"
                                MsgFormMessageErrorPasswordResetTokenExpired
                            redirect PasswordChangeR
                        else do
                            let userName = emailEmail e
                            setUltDest (PasswordResetR token)
                            $(widgetFile "auth/password-change")
            _ -> do
                messageRender <- getMessageRender
                urlRender <- getUrlRender
                let message = invalidTokenMessageWidget urlRender messageRender
                addMessage "token" (message)
                redirect PasswordChangeR
  where
    invalidTokenMessageWidget urlRender messageRender = [shamlet|
        #{messageRender MsgAPIInvalidPasswordChangeLink}
        <br>
        <small .text-muted>
            #{urlRender (PasswordResetR token)}
        |]

postApiUserPasswordChangeR :: Handler TypedContent
postApiUserPasswordChangeR = do
    msgRender <- getMessageRender :: Handler (AppMessage -> Text)
    user <- runInputPostResult $ ireq textField "username"
    pass <- runInputPostResult $ ireq (minLenPassField msgRender) "password"
    case (user, pass) of
        (FormSuccess login, FormSuccess passString) -> do
        -- TODO: FIXME: Check password length
            success <- runDB $ do
                creds <- getCredsByEmail login
                case creds of
                    ((Entity emailId _), (Entity userid _)):_ -> do
                        apiUnsafeChangeUserPassword userid passString
                        return $ Just emailId
                    _ -> do
                        return Nothing
            messageRender <- getMessageRender
            case success of
                Just emailId -> do
                    removeObsoleteTokens emailId
                    addMessage
                        "password"
                        (toHtml $ messageRender MsgFormSuccessPasswordSet)
                    setUltDest HomeR
                Nothing -> addMessage
                        "password"
                        (toHtml $ messageRender MsgFormMessageErrorPasswordChangeError)
            redirectUltDest HomeR
        (FormFailure loginErrors, FormFailure passwordErrors) ->
            addErrorsMessageRedirect
                    (loginErrors <> passwordErrors) PasswordChangeR
        (FormFailure errors, _) ->
            addErrorsMessageRedirect errors PasswordChangeR
        (_, FormFailure errors) ->
            addErrorsMessageRedirect errors PasswordChangeR
        (FormMissing, _) -> do
            addMessageI "form" MsgFormMessageErrorMissingLogin
            redirect PasswordChangeR
        (_, FormMissing) -> do
            addMessageI "form" MsgFormMessageErrorMissingPassword
            redirect PasswordChangeR
  where
    addErrorsMessageRedirect es url = do
        mapM (addMessageI "form") es
        redirectUltDest url

    minLenPassField msg = check (validatePass msg) passwordField

    minPassLen = 6 :: Int

    validatePass msg p
        | length p < minPassLen =
                (Left . msg) (MsgFormMessageErrorPasswordTooShort minPassLen)
        | otherwise  = Right p



createPasswordChangeToken :: Entity Email -> Handler (Entity PasswordResetToken)
createPasswordChangeToken (Entity emailId _) = do
    token <- appNonce128urlT
    now <- liftIO getCurrentTime
    let ltm = defaultTokenLifeTime
    let record = PasswordResetToken
            { passwordResetTokenEmail = emailId
            , passwordResetTokenToken = token
            , passwordResetTokenCreated = now
            , passwordResetTokenLifeTimeMinutes = ltm
            }
    recId <- runDB $ do
        -- Delete old tokens.  Only one token are valid at any time
        deleteWhere [ PasswordResetTokenEmail ==. emailId ]
        insert record
    return $ Entity recId record


withExistingLogin :: Text -> (Entity Email -> Handler Html) -> Handler Html
withExistingLogin email handler = do
    emailDb <- runDB . getBy $ UniqueEmail email
    case emailDb of
        Nothing -> do
            addMessageI
                "username"
                MsgFormMessageErrorInvalidLogin
            redirectUltDest HomeR
        Just e  -> handler e


-- | Default password reset token life-time, equals to 120 minutes.
defaultTokenLifeTime :: Int
defaultTokenLifeTime = 120

removeObsoleteTokens :: EmailId -> Handler ()
removeObsoleteTokens e = runDB $
    deleteWhere [ PasswordResetTokenEmail ==. e ]

refStatsW :: UserId -> Widget
refStatsW u = do
    stats' <- handlerToWidget . getUserReferrals $ u
    let stats = zip [1..] stats'
    [whamlet|
        <div .referral-stats>
            $forall (lvl, s) <- stats
                ^{refLevelStats lvl s}
        |]
  where
    refLevelStats _ [] = mempty
    refLevelStats n s = [whamlet|<div>
        _{MsgReferralLevel n}: #{show (length s)}|]

getUserReferrals :: UserId -> Handler [[Entity Referral]]
getUserReferrals user = do
    token <- appNonce128urlT
    ref <- runDB $ getCreateRefTokenDB user token
    getReferralsOf [entityKey ref]

getReferralsOf :: [ReferrerId] -> Handler [[Entity Referral]]
getReferralsOf refs = do
    maxLevels <- appRefMaxLevels . appSettings <$> getYesod
    foldReferrals maxLevels [] refs
  where
    foldReferrals n acc' refIds = do
        referrals <- runDB . getReferralsOfDB $ refIds
        let rels = map (\(rel, _, _) -> rel) referrals
            relRefs = map (\(_, _, Entity relRef _) -> relRef) referrals
        let acc = acc' <> [rels]
        if n == 0 || null rels
            then return acc
            else foldReferrals (n - 1) acc relRefs

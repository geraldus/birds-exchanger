{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Client.Settings where

import           Handler.API.Client.Password   ( apiUnsafeChangeUserPassword )
import           Import
import           Settings.MailRu               ( password, serverName, smtpPort,
                                                 username )
import           Utils.Database.Password       ( getCredsByEmail,
                                                 getCredsByToken )
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
    -- changePasswordElementRoot <- newIdent
    let passwordPrefix = take 6 <$> (userPassword . entityVal) user
    let shouldNoticeAboutPasswordChange = passwordPrefix /= Just "sha256"
    defaultLayout
        $(widgetFile "client/settings")

postApiUserPasswordChangeR :: Handler TypedContent
postApiUserPasswordChangeR = do
    -- client <- requireClientId
    msgRender <- getMessageRender :: Handler (AppMessage -> Text)
    user <- runInputPostResult $ ireq textField "username"
    pass <- runInputPostResult $ ireq (minLenPassField msgRender) "password"
    case (user, pass) of
        (FormSuccess login, FormSuccess passString) -> do
        -- TODO: FIXME: Check password length
            success <- runDB $ do
                creds <- getCredsByEmail login
                case creds of
                    ((Entity _ _), (Entity userid _)):_ -> do
                        apiUnsafeChangeUserPassword userid passString
                        return True
                    _ -> do
                        return False
            messageRender <- getMessageRender
            addMessage "password" $ if success
                then (toHtml . messageRender)
                        MsgFormSuccessPasswordSet
                else (toHtml . messageRender)
                        MsgFormMessageErrorPasswordChangeError
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
        liftIO $ do
            conn <- connectSMTPSSLWithSettings
                (unpack serverName)
                (defaultSettingsSMTPSSL { sslPort = smtpPort })
            authSuccess <-
                Network.HaskellNet.SMTP.SSL.authenticate
                    PLAIN
                    (unpack username)
                    (unpack password)
                    conn
            ret <- if authSuccess
                then do
                    let from = "noreply@outb.info"
                        subject = messageRender
                            MsgMessageClientPasswordResetTitle
                    sendMimeMail (unpack email)
                                  from
                                  (unpack subject)
                                  (textContent url)
                                  (htmlContent url homeUrl email)
                                  []
                                  conn
                    return []
                else do
                    let message = (toHtml . messageRender)
                            MsgAPIFailedToSendEmailAuthenticationFailed
                    return [("send-email", message)]
            closeSMTP conn
            return ret

    -- @TODO: Move to template file and use stFile QQ to embed
    textContent :: Text -> TL.Text
    textContent linkUrl =  fromStrict
            [stFile|templates/mail/password-reset-link.text|]

    htmlContent :: Text -> Text -> Text -> TL.Text
    htmlContent linkUrl siteUrl userName = renderHtml
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
                            removeObsoleteTokens eid
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

    removeObsoleteTokens :: EmailId -> Widget
    removeObsoleteTokens e = handlerToWidget . runDB $
        deleteWhere [ PasswordResetTokenEmail ==. e ]


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

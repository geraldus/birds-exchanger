{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Handler.LP.LP0001 where

import           Import                         hiding ( on, (==.) )

import           Handler.Client.Stocks.Purchase ( apiCreateUnconfirmedStocksPurchase,
                                                  jsonAddressee,
                                                  runStocksAmountPostForm )
import           Handler.SignUp                 ( addReferral, cleanUpRefCookie,
                                                  maybeReferrer )
import           Local.Persist.Currency         ( CryptoCurrency (PZM) )
import           Local.Persist.Notice           ( NoticeType (NoticeEmail) )
import           Local.Persist.TransferMethod   ( TransferMethod (CryptoTM) )
import           Local.Persist.UserRole         ( UserRole (Client) )
import           Settings.MailRu                ( projectNoReplyEmailCreds,
                                                  serverName, smtpPort )
import           Type.App                       ( defaultSelectNextAddr )
import           Utils.Common                   ( projectNameHost )
import           Utils.Database.Password        ( getCredsByEmail )
import           Utils.QQ                       ( stFile )
import           Yesod.Auth.Util.PasswordStore  ( makePassword )

import qualified Data.Aeson                     as A
import qualified Data.Text                      as T
import qualified Data.Text.Lazy                 as TL
import           Network.HaskellNet.SMTP
import           Network.HaskellNet.SMTP.SSL
import           Text.Blaze.Html.Renderer.Text  ( renderHtml )
import           Text.Hamlet                    ( shamletFile )


postLPHandler0001R :: Handler TypedContent
postLPHandler0001R = do
    stockRes <- runStocksAmountPostForm
    usrRes <- runQuickRegisterPostForm
    let res = (,) <$> stockRes <*> usrRes
    messageRenderer <- getMessageRender
    processFormResult res messageRenderer
  where
    processFormResult (Left es) render = selectRep $ do
        let message = MsgAPIInvalidFormData
        redirectWithMessages
            [ ("form", message) ]
            (map (\(l, e) -> (l, toHtml e)) es)
            HomeR
        provideRep . pure $ object
            [ "status" .= ("fail" :: Text)
            , "message" .= render message
            , "errors" .= toJSON es ]
    processFormResult (Right ((s, a, q), (u, e, pwd))) render = do
        addressee <-
            (jsonAddressee <$>) <$> getNextPaymentAddressee
                    defaultSelectNextAddr (CryptoTM PZM)
        case addressee of
            Nothing -> do
                let message = MsgFormErrorNoPaymentAddressee
                selectRep $ do
                    redirectWithMessages
                        [ ("form", message) ] [ ] HomeR
                    respondJSONErrorMessages
                        (render message) [ ]
            Just adr -> do
                purchaseDetails <-
                    apiCreateUnconfirmedStocksPurchase
                        s a q (entityKey u) adr
                let token = stocksPurchaseToken $
                        entityVal purchaseDetails
                case pwd of
                    Just pswd -> do
                        let lgn = (emailEmail . entityVal) e
                            creds = Creds "prizm auth plugin"
                                          lgn
                                          [("email", lgn)]
                        vk <- (flip fromMaybe (emailVerkey (entityVal e)))
                                <$> appNonce128urlT
                        cleanUpRefCookie
                        notifyClientQuickRegistrationCompleted lgn pswd vk
                        setCreds False creds
                    Nothing -> return ()
                selectRep $ do
                    let url = ClientStocksPurchaseDetailsR token
                    provideRep $ (redirect url :: Handler Html)
                    provideRep . pure  $ object
                        [ "status" .= ("OK" :: Text)
                        , "data" .= toJSON purchaseDetails ]

runQuickRegisterPostForm :: Handler
    (Either [(Text, Text)] (Entity User, Entity Email, Maybe Text))
runQuickRegisterPostForm = do
    mr <- getMessageRender
    res <- runInputPostResult $ toLower <$> ireq textField "email"
    processFormResult res mr
  where
    processFormResult FormMissing render = do
        let message = MsgAPIMissingFormData
        return $ Left [ ("form", render message) ]
    processFormResult (FormFailure es) render = do
        let message = MsgAPIInvalidFormData
        return . Left $
            [ ("form", render message) ]
            <> (map ((,) "form-error") es)
    processFormResult (FormSuccess email) _ = do
        ref        <- maybeReferrer
        rt         <- appNonce128urlT
        pwd        <- appNonce128urlT
        saltedPass <- liftIO $ decodeUtf8 <$> makePassword (encodeUtf8 pwd) 14
        vk         <- appNonce128urlT
        (u, e, newPass) <- runDB $
            queryGetCreateCreds email (saltedPass, vk) (ref, rt)
        let res = (u, e, if newPass then Just pwd else Nothing)
        return $ Right res


queryGetCreateCreds ::
       MonadIO m
    => Text
    -> (Text, Text)
    -> (Maybe (Entity Referrer), Text)
    -> SqlPersistT m (Entity User, Entity Email, Bool)
queryGetCreateCreds login' (pwd, vk) (ref, refToken) = do
    let login = toLower login'
    dbVals <- getCredsByEmail login
    case dbVals of
        []         -> createNewCreds login
        (e, u) : _ -> return $ (u, e, False)
  where
    -- ^ TODO: Combine with 'createUniqueLogin' in Handler.SignUp
    createNewCreds l = do
        let usr = User (toLower l) (Just pwd) Client
        uid <- insert usr
        let eml = Email (toLower l) (Just uid) (Just vk)
        eid <- insert eml
        _ <- case ref of
            Nothing -> return []
            Just r  -> (:[]) <$> addReferral r uid
        let refTok = Referrer uid refToken
        _ <- insert refTok
        return (Entity uid usr, Entity eid eml, True)


notifyClientQuickRegistrationCompleted :: Text -> Text -> Text -> Handler ()
notifyClientQuickRegistrationCompleted email pass vk = do
    projType      <- appType . appSettings <$> getYesod
    timeNow       <- liftIO getCurrentTime
    urlRender     <- getUrlRender
    messageRender <- getMessageRender
    let actUrl  = urlRender (SignUpVerifyR email vk)
        passUrl = urlRender PasswordChangeR
    let (_, exHost) = projectNameHost projType
    let subject  = unpack $
            messageRender MsgEmailSubjectQuickRegistrationCompleted
    let textContent' = textContent exHost pass actUrl passUrl
    let htmlContent' = htmlContent exHost pass actUrl passUrl
    let jsonContent = toStrict . decodeUtf8 . A.encode $ object
            [ "text-content" .= toJSON textContent'
            , "html-content" .= toJSON htmlContent' ]
    let notice = Notice
            NoticeEmail
            email
            jsonContent
            timeNow
            Nothing
            0
            Nothing
            (Just timeNow)
    nid <- runDB $ insert notice
    handler <- handlerToIO
    liftIO $ do
        conn <- connectSMTPSSLWithSettings
            (unpack serverName)
            (defaultSettingsSMTPSSL { sslPort = smtpPort })
        let (username, password) = projectNoReplyEmailCreds projType
        authSuccess <-
            Network.HaskellNet.SMTP.SSL.authenticate
                PLAIN
                (unpack username)
                (unpack password)
                conn
        if authSuccess
            then do
                sendMimeMail (T.unpack email)
                             (unpack username)
                             subject
                             textContent'
                             htmlContent'
                             []
                             conn
                handler . runDB $ update
                    nid
                    [ NoticeSent =. Just timeNow
                    , NoticeTrials +=. 1
                    , NoticeLastTrial =. Just timeNow
                    , NoticeNextTrial =. Nothing ]
            else putStrLn "Authentication failed."
        closeSMTP conn
  where
    textContent :: Text -> Text -> Text -> Text -> TL.Text
    textContent hostname password activationLink changePasswordLink =  fromStrict
        [stFile|templates/mail/quick-register.text|]

    htmlContent :: Text -> Text -> Text -> Text -> TL.Text
    htmlContent hostname password activationLink changePasswordLink = renderHtml
        $(shamletFile "templates/mail/quick-register.hamlet")



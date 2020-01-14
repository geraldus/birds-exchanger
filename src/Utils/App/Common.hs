{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Utils.App.Common where

import           Import

import           Settings                    ( AppType (..) )
import           Settings.MailRu             ( projectNoReplyEmailCreds,
                                               serverName, smtpPort )

import qualified Data.Text.Lazy              as TL
import           Network.HaskellNet.SMTP
import           Network.HaskellNet.SMTP.SSL


{- YESOD.  APP -}

getRenders :: WidgetFor App (Route App -> Text, AppMessage -> Text)
getRenders = (,) <$> liftHandler getUrlRender <*> liftHandler getMessageRender

setCompositeTitle ::
       (MonadWidget m, HandlerFor site ~ m, RenderMessage site msg)
    => [ msg ] -> m ()
setCompositeTitle ms = do
    r <- getMessageRender
    setTitle . toHtml . intercalate " | " . map r $ ms

-- | TODO Save Notice in database too
sendNoReplyEmail ::
       AppType
    -> (AppMessage -> Text)
    -> Text
    -> Text
    -> TL.Text
    -> TL.Text
    -> IO [(Text, Html)]
sendNoReplyEmail typ messageRender sendTo subj txt html = do
    let (username, password) = projectNoReplyEmailCreds typ
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
            sendMimeMail
                (unpack sendTo) (unpack username) (unpack subj) txt html [] conn
            return []
        else do
            let message = (toHtml . messageRender)
                    MsgAPIFailedToSendEmailAuthenticationFailed
            return [(("send-email" :: Text), message)]
    closeSMTP conn
    return ret

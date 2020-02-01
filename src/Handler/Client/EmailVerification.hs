{-# LANGUAGE OverloadedStrings #-}
module Handler.Client.EmailVerification where

import           Import

import           Local.Persist.Notice
import           Utils.App.Common              ( sendNoReplyEmail )
import           Utils.QQ                      ( stFile )

import qualified Data.Aeson                    as A
import qualified Data.Text.Lazy                as TL
import           Data.Time.Clock               ( NominalDiffTime, addUTCTime )
import           Text.Blaze.Html.Renderer.Text ( renderHtml )
import           Text.Hamlet                   ( shamletFile )
import           Utils.Common                  ( projectNameHost )


getClientRequestEmailVerificationR :: Handler Html
getClientRequestEmailVerificationR = do
    (Entity _ (Email email _ key), _) <- requireClientCreds
    case key of
        Nothing -> do
            addMessageI "email-validation" MsgMessageInfoEmailValidatedAlready
            redirectUltDest HomeR
        Just _ -> do
            htmlId <- newIdent
            defaultLayout $(widgetFile "messages/email-verification")


postClientRequestEmailVerificationR :: Handler TypedContent
postClientRequestEmailVerificationR = do
    (Entity _ (Email email _ key), _) <- requireClientCreds
    case key of
        Nothing -> do
            addMessageI "email-validation" MsgMessageInfoEmailValidatedAlready
            redirectUltDest HomeR
        Just verKey -> do
            proj          <- appType . appSettings <$> getYesod
            renderUrl     <- getUrlRender
            messageRender <- getMessageRender
            timeNow       <- liftIO getCurrentTime
            let projectName = fst $ projectNameHost proj
            let subject     = messageRender MsgEmailSubjectConfirmEmail
            let urlTitle    = messageRender MsgValidateEmail
            let url         = renderUrl $ SignUpVerifyR email verKey
            let txt         = textContent projectName url
            let html        = htmlContent projectName url urlTitle
            let jsonContent = toStrict . decodeUtf8 . A.encode $
                    object [ "subject"      .= toJSON subject
                           , "text-content" .= toJSON txt
                           , "html-content" .= toJSON html ]
            let n = Notice NoticeEmail
                           (Just NoticeSubjectEmailVerification)
                           email
                           jsonContent
                           timeNow
                           Nothing
                           0
                           Nothing
                           (Just timeNow)
            nid        <- runDB $ insert n
            handler    <- handlerToIO
            sendErrors <- liftIO $
                sendNoReplyEmail proj messageRender email subject txt html
            handler . runDB $ update nid $ case sendErrors of
                [] ->
                    [ NoticeSent =. Just timeNow
                    , NoticeTrials +=. 1
                    , NoticeLastTrial =. Just timeNow
                    , NoticeNextTrial =. Nothing ]
                _ ->
                    [ NoticeTrials +=. 1
                    , NoticeLastTrial =. Just timeNow
                    , NoticeNextTrial =. Just (addUTCTime fiveMinutes timeNow) ]
            case sendErrors of
                []     -> setMessage emailSentMessage
                errors -> mapM_ (\(l, e) -> addMessage l e) errors
            redirectUltDest HomeR
  where
    textContent :: Text -> Text -> TL.Text
    textContent projectName url = fromStrict
        [stFile|templates/mail/email-verification.text|]

    htmlContent :: Text -> Text -> Text -> TL.Text
    htmlContent projectName url urlTitle = renderHtml
        $(shamletFile "templates/mail/email-verification.hamlet")

    emailSentMessage :: Html
    emailSentMessage = $(shamletFile "templates/messages/email-verification-sent.hamlet")

    fiveMinutes :: NominalDiffTime
    fiveMinutes = 5 * 60

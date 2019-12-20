{-# LANGUAGE OverloadedStrings #-}
module Settings.MailRu where

import           ClassyPrelude
import           Network.Socket.Internal ( PortNumber )


-- | Your settings
serverName :: Text
serverName = "smtp.mail.ru"

smtpPort :: PortNumber
smtpPort = toEnum 465

usernameFenixNoreply :: Text
usernameFenixNoreply = "noreply@fenix.trading"

usernameOutbirdsNoreply :: Text
usernameOutbirdsNoreply = "noreply@outb.info"

password :: Text
password = "$afi2C3TFBsl"
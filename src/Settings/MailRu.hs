{-# LANGUAGE OverloadedStrings #-}
module Settings.MailRu where

import           ClassyPrelude
import           Network.Socket.Internal ( PortNumber )
import           Settings                ( AppType(..) )


-- | Your settings
serverName :: Text
serverName = "smtp.mail.ru"

smtpPort :: PortNumber
smtpPort = toEnum 465

supportEmailFenixUsername :: Text
supportEmailFenixUsername = "support@fenix.trading"

supportEmailFenixPassword :: Text
supportEmailFenixPassword = "gv0]Tr2FhvAR"

noreplyEmailFenixUsername :: Text
noreplyEmailFenixUsername = "noreply@fenix.trading"

noreplyEmailFenixPassword :: Text
noreplyEmailFenixPassword = "%9aO4OFqIjxc"

supportEmailOutbirdsUsername :: Text
supportEmailOutbirdsUsername = "support@outb.info"

supportEmailOutbirdsPassword :: Text
supportEmailOutbirdsPassword = "$afi2C3TFBsl"

noreplyEmailOutbirdsUsername :: Text
noreplyEmailOutbirdsUsername = "noreply@outb.info"

noreplyEmailOutbirdsPassword :: Text
noreplyEmailOutbirdsPassword = "T[fi20xTFlaX"


projectSupportEmailCreds :: AppType -> (Text, Text)
projectSupportEmailCreds FenixApp =
    (supportEmailOutbirdsUsername, supportEmailFenixPassword)
projectSupportEmailCreds OutbirdsApp =
    (supportEmailOutbirdsUsername, supportEmailOutbirdsPassword)

projectNoreplyEmailCreds :: AppType -> (Text, Text)
projectNoreplyEmailCreds FenixApp =
    (noreplyEmailFenixUsername, noreplyEmailFenixPassword)
projectNoreplyEmailCreds OutbirdsApp =
    (noreplyEmailOutbirdsUsername, noreplyEmailOutbirdsPassword)
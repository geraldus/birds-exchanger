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

-- noreplyEmailFenixUsername :: Text
-- noreplyEmailFenixUsername = "mailbot@fenix.trading"

-- noreplyEmailFenixPassword :: Text
-- noreplyEmailFenixPassword = "K,y00XBkYyhs"

supportEmailOutbirdsUsername :: Text
supportEmailOutbirdsUsername = "support@outb.info"

supportEmailOutbirdsPassword :: Text
supportEmailOutbirdsPassword = "$afi2C3TFBsl"

noreplyEmailOutbirdsUsername :: Text
noreplyEmailOutbirdsUsername = "noreply@outb.info"

noreplyEmailOutbirdsPassword :: Text
noreplyEmailOutbirdsPassword = "$afi2C3TFBsl"
-- noreplyEmailOutbirdsPassword = "T[fi20xTFlaX"

projectSupportEmailCreds :: AppType -> (Text, Text)
projectSupportEmailCreds FenixApp =
    (supportEmailFenixUsername, supportEmailFenixPassword)
projectSupportEmailCreds OutbirdsApp =
    (supportEmailOutbirdsUsername, supportEmailOutbirdsPassword)

projectNoReplyEmailCreds :: AppType -> (Text, Text)
projectNoReplyEmailCreds FenixApp =
    (noreplyEmailFenixUsername, noreplyEmailFenixPassword)
projectNoReplyEmailCreds OutbirdsApp =
    (noreplyEmailOutbirdsUsername, noreplyEmailOutbirdsPassword)
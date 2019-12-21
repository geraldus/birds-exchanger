{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Common handler functions.
module Handler.Common where

import           Data.FileEmbed ( embedFile )
import           Import

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR = do
    projType <- appType . appSettings <$> getYesod
    cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
    if projType == FenixApp
        then getFenixFaviconR
        else getOutbirdsFaviconR

getFenixFaviconR :: Handler TypedContent
getFenixFaviconR = return $ TypedContent "image/x-icon" $
    toContent $(embedFile "config/fenix-N-favicon.png")

getOutbirdsFaviconR :: Handler TypedContent
getOutbirdsFaviconR = return $ TypedContent "image/x-icon" $
    toContent $(embedFile "config/favicon.png")


getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")

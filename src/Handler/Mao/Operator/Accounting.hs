{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Mao.Operator.Accounting where

import           Import


getMaoAccountingR :: Handler Html
getMaoAccountingR = do
    let reactBuild =
#ifdef DEVELOPMENT
            "development"
#else
            "production.min"
#endif
    defaultLayout $ do
        addScriptRemote $ "https://unpkg.com/react@16/umd/react." <> reactBuild <> ".js"
        addScriptRemote $ "https://unpkg.com/react-dom@16/umd/react-dom." <> reactBuild <> ".js"
        $(widgetFile "mao/operator/accounting")
        addScriptAttrs (StaticR js_bundle_js) []

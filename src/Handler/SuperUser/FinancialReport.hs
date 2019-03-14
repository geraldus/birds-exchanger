{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.SuperUser.FinancialReport where

import           Import


getSuperUserFinancialReportViewR :: Handler Html
getSuperUserFinancialReportViewR = do
    requireSu
    renderUrl <- getUrlRender
    rm <- getMessageRender

    let reactBuild =
#ifdef DEVELOPMENT
            "development"
#else
            "production.min"
#endif
    defaultLayout $ do
        addScriptRemote $ "https://unpkg.com/react@16/umd/react." <> reactBuild <> ".js"
        addScriptRemote $ "https://unpkg.com/react-dom@16/umd/react-dom." <> reactBuild <> ".js"
        rootId <- newIdent
        $(widgetFile "su/financial-report")
        addScriptAttrs (StaticR js_bundle_js) []

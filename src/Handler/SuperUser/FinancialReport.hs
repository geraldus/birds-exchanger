{-# LANGUAGE OverloadedStrings #-}
module Handler.SuperUser.FinancialReport where

import           Import


getSuperUserFinancialReportViewR :: Handler Html
getSuperUserFinancialReportViewR = do
    su <- requireSu
    defaultLayout $ do
        addScriptRemote "https://unpkg.com/react@16/umd/react.development.js"
        addScriptRemote "https://unpkg.com/react-dom@16/umd/react-dom.development.js"
        addScriptAttrs (StaticR js_bundle_js) [("defer","defer")]
        [whamlet|<div #react-host>|]

{-# LANGUAGE QuasiQuotes #-}
module Handler.Manage.Info.Add (postManageInfoAddR) where

import Import

import           Utils.Time             ( renderDateTimeRow )

import           Database.Persist.Sql        ( fromSqlKey )


postManageInfoAddR :: Handler Html
postManageInfoAddR = do
    -- Entity _ info <- runDB $ getBy404 (UniqueInfoAlias alias)
    -- mr <- getMessageRender
    -- l <- selectLocale
    -- defaultLayout $ do
    --     setTitle $ toHtml $ infoTitle info <> " " <> mr MsgInfo
    --     [whamlet|
    --         <h1>#{infoTitle info}
    --         <p .mb-3>
    --             <small .text-muted>
    --                 #{renderDateTimeRow l (infoCreated info)}
    --         <div>
    --             #{preEscapedToMarkup (infoContentHtml info)}
    --         |]
    defaultLayout [whamlet||]
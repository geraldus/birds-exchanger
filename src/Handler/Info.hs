{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Info (
    getInfoListR
  , getInfoViewR ) where

import Import

import           Utils.Time             ( renderDateTimeRow )

import           Database.Persist.Sql        ( fromSqlKey )


getInfoListR :: Handler Html
getInfoListR = do
    infoItems <- runDB $ selectList [ ] [ Desc InfoCreated ]
    defaultLayout $ do
        setTitleI MsgInfo
        [whamlet|
            $case infoItems
                $of []
                    <h5>_{MsgNoPublicationsYet}
                $of list
                    <h5>_{MsgInfoListTitle}
                    ^{renderList list}
            |]
  where
    renderList :: [ Entity Info ] -> Widget
    renderList = mapM_ render'
    render' :: Entity Info -> Widget
    render' (Entity iid info) = do
        l <- handlerToWidget selectLocale
        [whamlet|
            <div .info-item #info-item#{fromSqlKey iid}>
                <a
                    title="_{MsgDetails}"
                    href="@{InfoViewR (infoAlias info)}">
                    #{infoTitle info}
                    <small .text-muted>
                        (#{renderDateTimeRow l (infoCreated info)})
            |]

getInfoViewR :: Text -> Handler Html
getInfoViewR alias = do
    Entity _ info <- runDB $ getBy404 (UniqueInfoAlias alias)
    mr <- getMessageRender
    l <- selectLocale
    defaultLayout $ do
        setTitle $ toHtml $ infoTitle info <> " " <> mr MsgInfo
        [whamlet|
            <h1>#{infoTitle info}
            <p .mb-3>
                <small .text-muted>
                    #{renderDateTimeRow l (infoCreated info)}
            <div>
                #{preEscapedToMarkup (infoContentHtml info)}
            |]
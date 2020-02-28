{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Editor.Articles.Index (getEditorArticlesIndexR) where

import           Import

import           Handler.Articles     ( articlePubDate )
import           Utils.Common         ( selectLocale )
import           Utils.Time

import           Database.Persist.Sql ( fromSqlKey )


getEditorArticlesIndexR :: Handler Html
getEditorArticlesIndexR = do
    _ <- requireEditorId
    items <- runDB $
        selectList [ ] [ Desc ArticleCreated, Desc ArticlePublishedAt ]
    mr <- getMessageRender
    defaultLayout $ do
        setTitle . toHtml $ mr MsgMenuTitleArticles <> " | " <> mr MsgManage
        [whamlet|
            <div .row .mb-5>
                <div .col-12>
                    <ul .nav .nav-pills>
                        <li .nav-item>
                            <a .nav-link .active href=@{EditorArticleAddR}>
                                _{MsgAdd}
            <div .row>
                <div .col>
                    $case items
                        $of []
                            <h5>_{MsgNoPublicationsYet}
                        $of list
                            <h5>_{MsgMenuTitleArticles}
                            <div .container-fluid>
                                <div .row>
                                    ^{renderList list}
            |]
  where
    renderList :: [ Entity Article ] -> Widget
    renderList = mapM_ render'

    render' :: Entity Article -> Widget
    render' (Entity aid a) = do
        l <- handlerToWidget selectLocale
        tzo <- handlerToWidget timezoneOffsetFromCookie
        [whamlet|
            <div .col-12 .article #article_#{fromSqlKey aid}>
                <a
                    title="_{MsgActionLabelArticleEdit}"
                    href="@{EditorArticleUpdateR aid}"
                    >
                    #{articleTitle a}
                <small .text-muted .text-lowercase>
                    #{renderDateTimeRow l tzo (articleCreated a) ["text-lowercase"]}
                <small .text-muted .ml-2>
                    $if articlePublished a
                        Публикация: #
                        #{renderDateTimeRow l tzo (articlePubDate a) ["text-lowercase"]}
                    $else
                        не опубликовано
            |]

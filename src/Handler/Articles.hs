{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Articles
  ( getArticlesListR
  , getArticleViewR
  , articlePubDate
  )
where

import           Import

import           Utils.Common                   ( selectLocale )
import           Utils.Time

import           Database.Persist.Sql           ( fromSqlKey )


getArticlesListR :: Handler Html
getArticlesListR = do
    infoItems' <- runDB $
        selectList
            [ ArticlePublished ==. True ]
            [ Desc ArticlePublishedAt, Desc ArticleCreated ]
    now <- liftIO getCurrentTime
    let infoItems = sortBy articlePubDateCmp $
            filter (alreadyPublished now) infoItems'
    defaultLayout $ do
        setAppPageTitle MsgArticlesListTitle
        [whamlet|
            $case infoItems
                $of []
                    <div>
                        <i>_{MsgNoPublicationsYet}
                $of list
                    <h1 .h5 .d-md-none>
                        _{MsgArticlesListTitle}
                    <div .container-fluid>
                        <div .row>
                            ^{renderList list}
            |]
 where
    renderList :: [Entity Article] -> Widget
    renderList = mapM_ render'

    render' :: Entity Article -> Widget
    render' (Entity aid a) = do
        l   <- handlerToWidget selectLocale
        tzo <- handlerToWidget timezoneOffsetFromCookie
        [whamlet|
            <div .col-12 .article #article_#{fromSqlKey aid}>
                <a
                    title="_{MsgDetails}"
                    href="@{ArticleViewR (articleAlias a)}">
                    #{articleTitle a}
                    <small .text-muted>
                        #{renderDateTimeRow l tzo (articlePubDate a) ["text-lowercase"]}
            |]

getArticleViewR :: Text -> Handler Html
getArticleViewR alias = do
  article@(Entity _ a) <- runDB $ getBy404 (UniqueArticleAlias alias)
  now                  <- liftIO getCurrentTime
  when (not (articlePublished a && alreadyPublished now article)) $ notFound
  l             <- selectLocale
  tzo           <- timezoneOffsetFromCookie
  titleIdent    <- newIdent
  contentIdent  <- newIdent
  thumbIdent    <- newIdent
  let title = fromMaybe (articleTitle a) (articleMetaTitle a)
  defaultLayout $ do
    setTitle . toHtml $ title
    [whamlet|
        <div ##{titleIdent}>
            <h1 .h5>#{articleTitle a}
            ^{dateRow l tzo (articlePubDate a)}
        $maybe t <- articleThumbUrl a
            <div ##{thumbIdent} .article-thumb .text-center .mb-5>
                <img src=#{t} alt="Обложка" style="max-width: 100%"/>
        <div ##{contentIdent} .article-content>
            #{preEscapedToMarkup (articleContentHtml a)}
        |]
    toWidget [cassius|
        ##{contentIdent} img
            max-width: 100%
        |]
 where
    dateRow l tzo date = [whamlet|
        <p .mb-3>
            <small .text-muted>
                #{renderDateTimeRow l tzo date ["text-lowercase"]}|]


articlePubDate :: Article -> UTCTime
articlePubDate a = if articlePublished a
    then fromMaybe (articleCreated a) (articlePublishedAt a)
    else articleCreated a

alreadyPublished :: UTCTime -> Entity Article -> Bool
alreadyPublished now (Entity _ a) = articlePubDate a <= now

articlePubDateCmp :: Entity Article -> Entity Article -> Ordering
articlePubDateCmp (Entity _ a1) (Entity _ a2) =
    articlePubDate a2 `compare` articlePubDate a1

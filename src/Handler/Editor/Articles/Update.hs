{-# LANGUAGE OverloadedStrings #-}
module Handler.Editor.Articles.Update (
      postAPIArticleUpdateR
    , getEditorArticleUpdateR
) where

import           Import

import           Data.Time.Clock.POSIX   ( posixSecondsToUTCTime )
import           Handler.Widgets.Widgets ( articleSimpleForm )


postAPIArticleUpdateR :: ArticleId -> Handler TypedContent
postAPIArticleUpdateR aid = do
    _           <- requireEditorId
    title       <- runInputPost $ ireq textField "title"
    alias       <- runInputPost $ ireq textField "alias"
    thumb       <- runInputPost $ iopt textField "thumb"
    published   <- runInputPost $ ireq checkBoxField "published"
    desc        <- runInputPost $ (fmap unTextarea) <$> iopt textareaField "desc"
    content     <- runInputPost $ ireq textField "content"
    metaTitle   <- runInputPost $ iopt textField "meta-title"
    metaDesc    <- runInputPost $ iopt textField "meta-desc"
    metaKWords  <- runInputPost $ iopt textField "meta-keywords"
    publishedAt <- runInputPost $ timestamp
        <$> iopt intField "published-at"
    runDB $ do
        get404 aid
        update
            aid
            [ ArticleTitle =. title
            , ArticleAlias =. alias
            , ArticleThumbUrl =. thumb
            , ArticlePublished =. published
            , ArticlePublishedAt =. publishedAt
            , ArticleMetaTitle =. metaTitle
            , ArticleMetaDesc =. metaDesc
            , ArticleMetaKWords =. metaKWords
            , ArticleDescHtml =. desc
            , ArticleContentHtml =. content]
    setMessageI MsgChangesSaved
    redirect $ EditorArticleUpdateR aid
  where
    timestamp = fmap (posixSecondsToUTCTime . (/ 1000) . fromIntegral)

getEditorArticleUpdateR :: ArticleId -> Handler Html
getEditorArticleUpdateR aid = do
    _  <- requireEditorId
    mr <- getMessageRender
    article <- runDB $ get aid
    defaultLayout $ do
        addScript (StaticR js_ckeditor5_ckeditor_js)
        $(widgetFile "editor/article-add")
        setTitle $ toHtml $ mr MsgMenuTitleArticles <> " | " <> mr MsgNewArticle
        let form = articleSimpleForm
                (titleVal article, Nothing)
                (aliasVal article, Nothing)
                (thumbUrlVal article, Nothing)
                (publishedVal article, Nothing)
                (publishedAtVal article, Nothing)
                (metaTitleVal article, Nothing)
                (metaDescVal article, Nothing)
                (metaKWordsVal article, Nothing)
                (descVal article, Nothing)
                (contentVal article, Nothing)
        [whamlet|
            <form method=post action=@{APIArticleUpdateR aid}>
                ^{form}
                <div .form-group>
                    <button #save-button .btn .btn-outline-primary .mt-2>
                        _{MsgSave}
            |]
  where
    titleVal = maybe "" articleTitle
    aliasVal = maybe "" articleAlias
    thumbUrlVal = maybe "" (fromMaybe "" . articleThumbUrl)
    publishedVal = maybe False articlePublished
    publishedAtVal = maybe Nothing articlePublishedAt
    metaTitleVal = maybe "" (fromMaybe "" . articleMetaTitle)
    metaDescVal = maybe "" (fromMaybe "" . articleMetaDesc)
    metaKWordsVal = maybe "" (fromMaybe "" . articleMetaKWords)
    descVal = maybe "" (fromMaybe "" . articleDescHtml)
    contentVal = maybe "" articleContentHtml



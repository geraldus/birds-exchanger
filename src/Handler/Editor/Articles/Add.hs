{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Editor.Articles.Add
    ( postAPIArticleAddR
    , getEditorArticleAddR)
where

import           Data.Time.Clock.POSIX   ( posixSecondsToUTCTime )
import           Handler.Widgets.Widgets
import           Import


getEditorArticleAddR :: Handler Html
getEditorArticleAddR = do
    _  <- requireEditorId
    mr <- getMessageRender
    defaultLayout $ do
        addScript (StaticR js_ckeditor5_ckeditor_js)
        $(widgetFile "editor/article-add")
        setTitle $ toHtml $ mr MsgMenuTitleArticles <> " | " <> mr MsgNewArticle
        let form = articleSimpleForm
                ("", Nothing)
                ("", Nothing)
                ("", Nothing)
                (False, Nothing)
                (Nothing, Nothing)
                ("", Nothing)
                ("", Nothing)
                ("", Nothing)
                ("", Nothing)
                ("", Nothing)
        [whamlet|
            <form method=post action=@{APIArticleAddR}>
                ^{form}
                <div .form-group>
                    <button #save-button .btn .btn-outline-primary .mt-2>
                        _{MsgSave}
            |]

postAPIArticleAddR :: Handler TypedContent
postAPIArticleAddR = do
    _           <- requireEditorId
    title       <- runInputPost $ ireq textField "title"
    alias       <- runInputPost $ ireq textField "alias"
    thumb       <- runInputPost $ iopt textField "thumb"
    content     <- runInputPost $ ireq textField "content"
    desc        <- runInputPost $ iopt textField "desc"
    published   <- runInputPost $ ireq checkBoxField "published"
    publishedAt <- runInputPost $ timestamp
        <$> iopt intField "published-at"
    metaTitle   <- runInputPost $ iopt textField "meta-title"
    metaDesc    <- runInputPost $ iopt textField "meta-desc"
    metaKWords  <- runInputPost $ iopt textField "meta-keywords"
    time         <- liftIO getCurrentTime
    article <- runDB $ insertEntity Article
        { articleTitle       = title
        , articleAlias       = alias
        , articleContentHtml = content
        , articleCreated     = time
        , articlePublished   = published
        , articleThumbUrl    = thumb
        , articleDescHtml    = desc
        , articlePublishedAt = publishedAt
        , articleMetaTitle   = metaTitle
        , articleMetaDesc    = metaDesc
        , articleMetaKWords  = metaKWords
        }
    selectRep $ do
        provideRep . pure $ object
            [ "status" .= ("ok" :: Text)
            , "data" .= toJSON article ]
        provideRep $ do
            setMessageI MsgChangesSaved
            (redirect $
                EditorArticleUpdateR (entityKey article) :: Handler Html)
  where
    timestamp = fmap (posixSecondsToUTCTime . (/ 1000) . fromIntegral)

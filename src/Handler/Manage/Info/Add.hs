{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Manage.Info.Add (postManageInfoAddR, getManageInfoAddR) where

import           Import

import Handler.Widgets.Widgets


getManageInfoAddR :: Handler Html
getManageInfoAddR = do
    mr <- getMessageRender
    defaultLayout $ do
        addScript (StaticR js_ckeditor5_ckeditor_js)
        $(widgetFile "editor/info-add")
        setTitle $ toHtml $ mr MsgInfo <> " | " <> mr MsgNewArticle
        let form = infoArticleSimpleForm
                ("", Nothing)
                ("", Nothing)
                ("", Nothing)
                (False, Nothing)
                ("", Nothing)
                ("", Nothing)
        [whamlet|
            <form method=post action=@{ManageInfoAddR}>
                ^{form}
                <div .form-group>
                    <button #save-button .btn .btn-outline-primary .mt-2>_{MsgSave}
            |]

postManageInfoAddR :: Handler Html
postManageInfoAddR = do
    title    <- runInputPost $ ireq textField "title"
    alias    <- runInputPost $ ireq textField "alias"
    thumb    <- runInputPost $ iopt textField "thumb"
    content  <- runInputPost $ ireq textField "content"
    desc     <- runInputPost $ iopt textField "desc"
    featured <- runInputPost $ ireq checkBoxField "featured"
    time     <- liftIO getCurrentTime
    runDB $ insert Info
        { infoTitle = title
        , infoAlias = alias
        , infoContentHtml = content
        , infoCreated = time
        , infoFeatured = featured
        , infoThumbUrl = thumb
        , infoDescHtml = desc
        }
    setMessageI MsgChangesSaved
    redirect $ InfoViewR alias

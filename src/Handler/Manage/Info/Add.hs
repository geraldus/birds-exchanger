{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Manage.Info.Add (postManageInfoAddR, getManageInfoAddR) where

import Import


getManageInfoAddR :: Handler Html
getManageInfoAddR = do
    mr <- getMessageRender
    defaultLayout $ do
        addScriptRemote "https://cdn.ckeditor.com/ckeditor5/11.2.0/classic/ckeditor.js"
        $(widgetFile "editor/info-add")
        setTitle $ toHtml $ mr MsgInfo <> " | " <> mr MsgNewArticle
        [whamlet|
            <form method=post action=@{ManageInfoAddR}>
                <div .form-group>
                    <input #title-input name="title" .form-control.form-control-lg type="text" required=required placeholder="_{MsgTitle}"/>
                <div .form-group>
                    <input #alias-input name="alias" .form-control type="text" required=required placeholder="_{MsgAlias}"/>
                <div .form-group>
                    <div #content-editor>
                <div .form-group>
                    <input #content-data name="content" type="hidden"/>
                    <button #save-button .btn .btn-outline-primary .mt-2>_{MsgSave}
            |]

postManageInfoAddR :: Handler Html
postManageInfoAddR = do
    title   <- runInputPost $ ireq textField "title"
    alias   <- runInputPost $ ireq textField "alias"
    content <- runInputPost $ ireq textField "content"
    time <- liftIO getCurrentTime
    runDB $ insert Info
        { infoTitle = title
        , infoAlias = alias
        , infoContentHtml = content
        , infoCreated = time }
    setMessageI MsgChangesSaved
    redirect $ InfoViewR alias
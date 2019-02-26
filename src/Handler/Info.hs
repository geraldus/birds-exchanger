{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Info (
    getInfoListR
  , getInfoViewR ) where

import           Import

import           Local.Persist.UserRole ( UserRole (..) )
import           Utils.Time             ( renderDateTimeRow )

import           Database.Persist.Sql   ( fromSqlKey )
import           Text.Julius            ( RawJS (..) )


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
        tzo <- handlerToWidget timezoneOffsetFromCookie
        [whamlet|
            <div .info-item #info-item#{fromSqlKey iid}>
                <a
                    title="_{MsgDetails}"
                    href="@{InfoViewR (infoAlias info)}">
                    #{infoTitle info}
                    <small .text-muted>
                        (#{renderDateTimeRow l tzo (infoCreated info)})
            |]

getInfoViewR :: Text -> Handler Html
getInfoViewR alias = do
    Entity infoId info <- runDB $ getBy404 (UniqueInfoAlias alias)
    mr <- getMessageRender
    l <- selectLocale
    tzo <- timezoneOffsetFromCookie
    mayUser <- maybeAuthPair
    let isEditorLoggedIn = maybe False (isEditor . snd) mayUser
    titleIdent <- newIdent
    aliasIdent <- newIdent
    contentIdent <- newIdent
    defaultLayout $ do
        when isEditorLoggedIn $ do
            addScriptRemote "https://cdn.ckeditor.com/ckeditor5/11.2.0/inline/ckeditor.js"
            $(widgetFile "editor/info-update")
        setTitle $ toHtml $ infoTitle info <> " " <> mr MsgInfo
        [whamlet|
            <div ##{titleIdent}>
                $if isEditorLoggedIn
                    <div .form-group>
                        <input #title-input .form-control.form-control-lg type="text" required=required value="#{infoTitle info}"/>
                    <div .form-group .row>
                        <label .col-form-label .col-2 for="alias-input">Ссылка на статью
                        <div .col-10>
                            <input #alias-input .form-control type="text" required=required value="#{infoAlias info}"/>
                $else
                    <h1>#{infoTitle info}

            <p .mb-3>
                <small .text-muted>
                    #{renderDateTimeRow l tzo (infoCreated info)}
            <div ##{contentIdent} .info-content>
                #{preEscapedToMarkup (infoContentHtml info)}
            $if isEditorLoggedIn
                <form method=post action=@{ManageInfoUpdateR}>
                    <input type=hidden name="info-id" value="#{fromSqlKey infoId}"/>
                    <input ##{titleIdent}-data type=hidden name="title" value="#{infoTitle info}"/>
                    <input ##{aliasIdent}-data type=hidden name="alias" value="#{infoAlias info}"/>
                    <input ##{contentIdent}-data type=hidden name="content" value="#{infoContentHtml info}"/>
                    <button #save-button .btn .btn-outline-primary .mt-2>_{MsgSave}
            |]
        toWidget [cassius|
            ##{contentIdent} img
                max-width: 100%
            |]
  where
    isEditor (Right _) = True
    isEditor (Left u)  = userRole u == Editor
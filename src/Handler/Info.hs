{-# LANGUAGE OverloadedStrings #-}{-# LANGUAGE QuasiQuotes       #-}
module Handler.Info
  ( getInfoListR
  , getInfoViewR
  )
where

import           Import

import           Local.Persist.UserRole         ( UserRole(..) )
import           Utils.Common                   ( selectLocale )
import           Utils.Time
import           Handler.Widgets.Widgets

import           Database.Persist.Sql           ( fromSqlKey )
import           Text.Julius                    ( RawJS(..) )


getInfoListR :: Handler Html
getInfoListR = do
  infoItems <- runDB $ selectList [] [Desc InfoCreated]
  defaultLayout $ do
    setAppPageTitle MsgInfo
    [whamlet|
            $case infoItems
                $of []
                    <h5>_{MsgNoPublicationsYet}
                $of list
                    <h5>_{MsgInfoListTitle}
                    ^{renderList list}
            |]
 where
  renderList :: [Entity Info] -> Widget
  renderList = mapM_ render'
  render' :: Entity Info -> Widget
  render' (Entity iid info) = do
    l   <- handlerToWidget selectLocale
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
  mr                 <- getMessageRender
  l                  <- selectLocale
  tzo                <- timezoneOffsetFromCookie
  mayUser            <- maybeAuthPair
  let isEditorLoggedIn = maybe False (isEditor . snd) mayUser
  titleIdent    <- newIdent
  aliasIdent    <- newIdent
  contentIdent  <- newIdent
  descIdent     <- newIdent
  featuredIdent <- newIdent
  thumbIdent    <- newIdent
  let form = infoArticleSimpleForm
        ((infoTitle info)                  , Just titleIdent)
        ((infoAlias info)                  , Just aliasIdent)
        ((fromMaybe "" (infoThumbUrl info)), Just thumbIdent)
        ((infoFeatured info)               , Just featuredIdent)
        ( infoContentHtml info , Just contentIdent)
        ( fromMaybe "" (infoDescHtml info) , Just descIdent)
  defaultLayout $ do
    when isEditorLoggedIn $ do
        addScriptRemote
            "https://cdn.ckeditor.com/ckeditor5/11.2.0/classic/ckeditor.js"
        -- addScriptRemote "https://cdn.ckeditor.com/ckeditor5/12.4.0/inline/ckeditor.js"
    setTitle $ toHtml $ infoTitle info <> " " <> mr MsgInfo
    [whamlet|
        $if isEditorLoggedIn
            <form method=post action=@{ManageInfoUpdateR}>
                Дата создания:<br>
                ^{dateRow l tzo (infoCreated info)}
                <input
                    type=hidden
                    name="info-id"
                    value="#{fromSqlKey infoId}"
                    />
                ^{form}
                <button
                    #save-button
                    .btn
                    .btn-outline-primary
                    .mt-2
                    >
                    _{MsgSave}
            <div>
        $else
            <div ##{titleIdent}>
                <h1>#{infoTitle info}
                ^{dateRow l tzo (infoCreated info)}
            $maybe t <- infoThumbUrl info
                <div ##{thumbIdent} .info-thumb>
                    <img src=#{t} alt="Обложка" style="max-width: 100%"/>
            <div ##{contentIdent} .info-content>
                #{preEscapedToMarkup (infoContentHtml info)}
        |]
    $(widgetFile "editor/info-update")
    toWidget [cassius|
            ##{contentIdent} img
                max-width: 100%
            |]
 where
  isEditor (Right _) = True
  isEditor (Left  u) = userRole u == Editor

  dateRow l tzo date = [whamlet|
        <p .mb-3>
            <small .text-muted>
                #{renderDateTimeRow l tzo date}|]

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Manage.Info.Index (getManageInfoIndexR) where

import           Import

import           Utils.Time

import           Database.Persist.Sql ( fromSqlKey )


getManageInfoIndexR :: Handler Html
getManageInfoIndexR = do
    infoItems <- runDB $ selectList [ ] [ Desc InfoCreated ]
    mr <- getMessageRender
    defaultLayout $ do
        setTitle . toHtml $ mr MsgInfo <> " | " <> mr MsgManage
        [whamlet|
            <div .row .mb-5>
                <ul .nav .nav-pills>
                    <li .nav-item>
                        <a .nav-link .active href=@{ManageInfoAddR}>_{MsgAdd}

            <div .row>
                <div .col>
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

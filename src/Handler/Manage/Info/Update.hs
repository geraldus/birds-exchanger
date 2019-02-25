{-# LANGUAGE OverloadedStrings #-}
module Handler.Manage.Info.Update
    ( postManageInfoUpdateR
    )
where

import           Import

import           Database.Persist.Sql           ( toSqlKey )


postManageInfoUpdateR :: Handler Html
postManageInfoUpdateR = do
    infoId  <- toSqlKey <$> runInputPost (ireq intField "info-id")
    title   <- runInputPost $ ireq textField "title"
    alias   <- runInputPost $ ireq textField "alias"
    content <- runInputPost $ ireq textField "content"
    runDB $ do
        get404 infoId
        update
            infoId
            [InfoTitle =. title, InfoAlias =. alias, InfoContentHtml =. content]
    setMessageI MsgChangesSaved
    redirect $ InfoViewR alias

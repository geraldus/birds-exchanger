{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Admin.Users.Index where

import           Import

import           Handler.API.Admin.Users.List ( apiAdminDoesUserExists,
                                                apiAdminSafelyCreateUser,
                                                apiAdminSafelyUpdateUser,
                                                apiAdminUsersList )
import           Local.Persist.UserRole

import           Data.Text                    ( strip )
import qualified Data.Text                    as T ( replace )
import           Text.Julius                  ( RawJS (rawJS) )


getAdminUsersListR :: Handler Html
getAdminUsersListR = do
    _ <- requireAdminId
    vals <- apiAdminUsersList
    let list = [whamlet|
            <table .table .table-hover .hover>
                $forall i <- vals
                    ^{renderUser i}
            |]
    defaultLayout $(widgetFile "admin/users/index")
  where
    renderUser (Entity uid u) = $(widgetFile "admin/users/item")

    renderUserRole :: UserRole -> Text
    renderUserRole Client   = "Клиент"
    renderUserRole Editor   = "Редактор"
    renderUserRole Operator = "Оператор"
    renderUserRole Admin    = "Администратор"

getAdminUsersCreateR :: Handler Html
getAdminUsersCreateR = do
    _ <- requireAdminId
    defaultLayout $(widgetFile "admin/users/create")

getAdminUsersUpdateR :: UserId -> Handler Html
getAdminUsersUpdateR uid = do
    _ <- requireAdminId
    userData <- runDB $ get uid
    case userData of
        Nothing -> do
            addMessage "user-form" "Пользователь не найден"
            redirect AdminUsersListR
        Just user -> defaultLayout $(widgetFile "admin/users/update")

postAdminUsersCreateR :: Handler Html
postAdminUsersCreateR = do
    _ <- requireAdminId
    (username, usertype, password) <- runPostForm
    created <- apiAdminSafelyCreateUser username usertype password
    case created of
        Nothing -> do
            addMessage "user-form" "Ошибка создания пользователя."
            redirect AdminUsersCreateR
        Just (Entity _ usr, _) -> do
            addMessage "user-form" $ toHtml $
                "Пользователь " <> (userIdent usr) <> " создан."
            redirect AdminUsersCreateR
  where
    runPostForm = runInputPost $ (,,)
        <$> (strip <$> ireq textField "ident")
        <*> (textToUserRoleUnsafe <$> ireq textField "usertype")
        <*> ireq textField "password"

postAdminUsersUpdateR :: UserId -> Handler Html
postAdminUsersUpdateR uid = do
    _ <- requireAdminId
    (username, usertype, password) <- runPostForm
    created <- apiAdminSafelyUpdateUser uid username usertype password
    case created of
        Nothing -> do
            addMessage "user-form" "Ошибка обновления пользователя."
            redirect AdminUsersCreateR
        Just (Entity _ usr, _) -> do
            addMessage "user-form" $ toHtml $
                "Пользователь " <> (userIdent usr) <> " изменён."
            redirect (AdminUsersUpdateR uid)
  where
    runPostForm = runInputPost $ (,,)
        <$> (strip <$> ireq textField "ident")
        <*> (textToUserRoleUnsafe <$> ireq textField "usertype")
        <*> iopt textField "password"

textToUserRoleUnsafe :: Text -> UserRole
textToUserRoleUnsafe "Client"   = Client
textToUserRoleUnsafe "Editor"   = Editor
textToUserRoleUnsafe "Operator" = Operator
textToUserRoleUnsafe "Admin"    = Admin
textToUserRoleUnsafe _          = error "Неверный тип пользователя"


postAPIAdminUsersDoesUserExistsR :: Handler TypedContent
postAPIAdminUsersDoesUserExistsR = do
    _ <- requireAdminId
    maybeUser <- apiAdminDoesUserExists
    selectRep $ provideRep $
        case maybeUser of
            Nothing -> return $ object
                [ "status" .= ("ok" :: Text)
                , "result" .= toJSON False ]
            Just user -> return $ object
                [ "status" .= ("ok" :: Text)
                , "result" .= toJSON True
                , "data"   .= toJSON user ]

spacesToNbSp :: Text -> Text
spacesToNbSp = T.replace " " "&nbsp;"

{-# LANGUAGE OverloadedStrings #-}

module Handler.Widgets.Widgets where

import Import


textInputWidget :: Text -> Text -> Bool -> Text -> AppMessage -> Widget
textInputWidget id' name required value placeholder =
    [whamlet|
        <input
            ##{id'}
            name="#{name}"
            .form-control
            .form-control-lg
            type="text"
            :required:required=required
            value="#{value}"
            placeholder="_{placeholder}"
            />
        |]


infoArticleSimpleForm
    :: (Text, Maybe Text)
    -> (Text, Maybe Text)
    -> (Text, Maybe Text)
    -> (Bool, Maybe Text)
    -> (Text, Maybe Text)
    -> (Text, Maybe Text)
    -> Widget
infoArticleSimpleForm
    (title, titleIdent)
    (alias, aliasIdent)
    (thumb, thumbIdent)
    (featured, featuredIdent)
    (content, contentIdent)
    (desc, descIdent) =
    [whamlet|
        <div .form-group>
            ^{titleInput}
        <div .form-group>
            ^{aliasInput}
        <div .form-group .mb-3>
            <label for="#{thumbIdent'}">Обложка
            ^{thumbInput}
        <div .form-check .mb-3>
            <input
                ##{featuredIdent'}
                name="featured"
                .form-check-input
                type="checkbox"
                :featured:checked="checked"
                />
            <label .form-check-label for="#{featuredIdent'}">
                Продивигать новость
        <div .form-group .mb-3>
            <label for="#{descIdent'}-editor">Короткое описание
            <div ##{descIdent'}-editor>
                #{preEscapedToMarkup desc}
        <div .form-group>
            <label for="#{contentIdent'}-editor">Текст новости
            <div ##{contentIdent'}-editor>
                #{preEscapedToMarkup content}
        <div .form-group>
            <input
                ##{contentIdent'}-data
                name="content"
                type="hidden"
                value='#{content}'
                />
            <input
                ##{descIdent'}-data
                name="desc"
                value='#{desc}'
                type="hidden"/>
            |]
    where
        titleIdent' = fromMaybe "title-input" titleIdent
        titleInput = textInputWidget
                titleIdent' "title" True title MsgTitle

        aliasIdent' = fromMaybe "alias-input" aliasIdent
        aliasInput = textInputWidget
                aliasIdent' "alias" True alias MsgAlias

        thumbIdent' = fromMaybe "thumb-input" thumbIdent
        thumbInput = textInputWidget
                thumbIdent' "thumb" False thumb MsgThumbUrl

        featuredIdent' = fromMaybe "featured-input" featuredIdent

        contentIdent' = fromMaybe "content-html" contentIdent

        descIdent' = fromMaybe "desc-html" descIdent

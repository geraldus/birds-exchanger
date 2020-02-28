{-# LANGUAGE OverloadedStrings #-}

module Handler.Widgets.Widgets where

import           Data.Time.Clock.POSIX ( utcTimeToPOSIXSeconds )
import           Import


textInputWidget :: Text -> Text -> Bool -> Bool -> Text -> AppMessage -> Widget
textInputWidget id' name required lg value placeholder =
    [whamlet|
        <input
            ##{id'}
            name="#{name}"
            .form-control
            :lg:.form-control-lg
            type="text"
            :required:required=required
            value="#{value}"
            placeholder="_{placeholder}"
            />
        |]


infoArticleSimpleForm ::
       (Text, Maybe Text)
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
                value=#{content}
                />
            <input
                ##{descIdent'}-data
                name="desc"
                value=#{desc}
                type="hidden"/>
            |]
    where
        titleIdent' = fromMaybe "title-input" titleIdent
        titleInput = textInputWidget
                titleIdent' "title" True True title MsgTitle

        aliasIdent' = fromMaybe "alias-input" aliasIdent
        aliasInput = textInputWidget
                aliasIdent' "alias" False True alias MsgAlias

        thumbIdent' = fromMaybe "thumb-input" thumbIdent
        thumbInput = textInputWidget
                thumbIdent' "thumb" False False thumb MsgThumbUrl

        featuredIdent' = fromMaybe "featured-input" featuredIdent

        contentIdent' = fromMaybe "content-html" contentIdent

        descIdent' = fromMaybe "desc-html" descIdent


articleSimpleForm ::
       (Text, Maybe Text)
    -> (Text, Maybe Text)
    -> (Text, Maybe Text)
    -> (Bool, Maybe Text)
    -> (Maybe UTCTime, Maybe Text)
    -> (Text, Maybe Text)
    -> (Text, Maybe Text)
    -> (Text, Maybe Text)
    -> (Text, Maybe Text)
    -> (Text, Maybe Text)
    -> Widget
articleSimpleForm
    (title, titleIdent)
    (alias, aliasIdent)
    (thumb, thumbIdent)
    (published, publishedIdent)
    (publishedAt, publishedAtIdent)
    (metaTitle, metaTitleIdent)
    (metaDesc, metaDescIdent)
    (metaKWords, metaKWordsIdent)
    (desc, descIdent)
    (content, contentIdent) = do
    addStylesheet
        (StaticR _3rd_party_air_datepicker_2_2_3_dist_css_datepicker_min_css)
    addScript
        (StaticR _3rd_party_air_datepicker_2_2_3_dist_js_datepicker_min_js)
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
                ##{publishedIdent'}
                name="published"
                .form-check-input
                type="checkbox"
                :published:checked="checked"
                />
            <label .form-check-label for="#{publishedIdent'}">
                _{MsgFormLabelArticlePublished}
        <div .form-group .row>
            <label for="#{publishedAtIdent'}" .col-2>
                _{MsgFormLabelPublishedAt}
            <input
                ##{publishedAtIdent'}
                .form-control
                .datepicker-here
                .col-10
                type=text
                data-position="bottom left"
                data-timepicker="true"
                value="#{maybe "" show publishedAt}"
                >
            <input
                ##{publishedAtIdent'}-data
                type=hidden
                name="published-at"
                value=#{publishedAtSeconds}
                >
        <div .form-group>
            <label for="#{metaTitleIdent'}">
                <small .text-muted>META
                _{MsgFormLabelSEOMetaTitle}
            ^{metaTitleInput}
        <div .form-group>
            <label for="#{metaKWordsIdent'}">
                <small .text-muted>META
                _{MsgFormLabelSEOMetaKeywords}
            ^{metaKWordsInput}
        <div .form-group .mb-3>
            <label for="#{metaDescIdent'}-data">
                <small .text-muted>META
                Описание
            <textarea
                ##{metaDescIdent'}-data
                .form-control
                name="meta-desc"
                />
                #{metaDesc}
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
                value=#{content}
                />
            <input
                ##{descIdent'}-data
                name="desc"
                value=#{desc}
                type="hidden"/>
        |]
  where
    titleIdent' = fromMaybe "title-input" titleIdent
    titleInput = textInputWidget
            titleIdent' "title" True True title MsgTitle

    aliasIdent' = fromMaybe "alias-input" aliasIdent
    aliasInput = textInputWidget
            aliasIdent' "alias" True False alias MsgAlias

    thumbIdent' = fromMaybe "thumb-input" thumbIdent
    thumbInput = textInputWidget
            thumbIdent' "thumb" False False thumb MsgThumbUrl

    publishedIdent' = fromMaybe "published-input" publishedIdent

    publishedAtIdent' = fromMaybe "published-at-input" publishedAtIdent
    publishedAtSeconds = maybe
        "" (show . realToFrac .  (* 1000) . utcTimeToPOSIXSeconds) publishedAt

    metaDescIdent' = fromMaybe "meta-desc-input" metaDescIdent

    metaTitleIdent' = fromMaybe "meta-title-input" metaTitleIdent
    metaTitleInput = textInputWidget
            metaTitleIdent' "meta-title" False False metaTitle MsgTitle

    metaKWordsIdent' = fromMaybe "meta-keywords-input" metaKWordsIdent
    metaKWordsInput = textInputWidget
        metaKWordsIdent'
        "meta-keywords"
        False
        False
        metaKWords
        MsgFormPlaceholderSEOMetaKeywords

    contentIdent' = fromMaybe "content-html" contentIdent

    descIdent' = fromMaybe "desc-html" descIdent

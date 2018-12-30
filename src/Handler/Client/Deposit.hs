{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Handler.Client.Deposit where


import           Form.Profile.Deposit
import           Import
import           Local.Persist.Deposit


getDepositR :: Handler Html
getDepositR = do
    requireClientId
    formId <- newIdent
    (widget, enctype) <- generateFormPost $ depositForm formId
    defaultLayout $ defaultWidget formId widget enctype Nothing

postDepositR :: Handler Html
postDepositR = do
    userId <- requireClientId
    formId <- newIdent
    ((res, widget), enctype) <- runFormPost $ depositForm formId
    let mayError = case res of
            FormSuccess _ -> Nothing
            FormMissing   -> Just  ["Не получены данные формы"]
            FormFailure e -> Just e
    case res of
        FormMissing -> defaultLayout $ defaultWidget formId widget enctype mayError
        FormFailure _ -> defaultLayout $ defaultWidget formId widget enctype mayError
        FormSuccess DepositRequestFD{..} -> do
            code <- appNonce128urlT
            time <- liftIO getCurrentTime
            let depReqRecord = DepositRequest
                    depReqCurrency
                    depReqTransferMethod
                    depReqCentsAmount
                    depReqCentsExpectedFee
                    code
                    depReqTargetCurrency
                    depReqExpectedConversionRatio
                    time
                    userId
                    New
                    False
            _ <- runDB $ insert depReqRecord
            redirect $ DepositRequestConfirmationR code

defaultWidget :: Text -> Widget -> Enctype -> Maybe [Text] -> Widget
defaultWidget formId widget enctype mayError = [whamlet|
    <form ##{formId} method=post enctype=#{enctype} .col-6 .mx-auto>
        ^{widget}
        $maybe error <- mayError
            <div .alert .alert-danger role="alert">
                $forall e <- error
                    <div .error>#{e}
        <div .form-group .row>
            <button type=submit .btn.btn-outline-primary.btn-lg .mx-auto>продолжить
    |]

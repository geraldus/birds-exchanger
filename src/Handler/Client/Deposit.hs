{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Client.Deposit where


import Import
import Local.Persist.Currency
import Local.Persist.Deposit
import Form.Profile.Deposit


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
    mayError <- return $ case res of
        FormSuccess depReq -> Nothing
        FormMissing -> Just  ["Не получены данные формы"]
        FormFailure e -> Just e
    case res of
        FormMissing -> defaultLayout $ defaultWidget formId widget enctype mayError
        FormFailure _ -> defaultLayout $ defaultWidget formId widget enctype mayError
        FormSuccess DepositRequestFD{..} -> do
            code <- appNonce128urlT
            time <- liftIO $ getCurrentTime
            let depReqRecord = DepositRequest
                    depReqCurrency
                    depReqPaymentMethod
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
defaultWidget formId widget enctype mayError = do
    [whamlet|
        <form ##{formId} method=post enctype=#{enctype} .col-6 .mx-auto>
            ^{widget}
            $maybe error <- mayError
                <div .alert .alert-danger role="alert">
                    $forall e <- error
                        <div .error>#{e}
            <div .form-group .row>
                <button type=submit .btn.btn-success.btn-lg .mx-auto>Продолжить
        |]
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Profile.Deposit where


import Import
import Local.Persist.Currency
import Local.Persist.Deposit
import Form.Profile.Deposit


getDepositR :: Handler Html
getDepositR = do
    requireClientId
    (widget, enctype) <- generateFormPost depositForm
    defaultLayout $ defaultWidget widget enctype Nothing

postDepositR :: Handler Html
postDepositR = do
    userId <- requireClientId
    ((res, widget), enctype) <- runFormPost depositForm
    mayError <- return $ case res of
        FormSuccess depReq -> Nothing
        FormMissing -> Just  ["Не получены данные формы"]
        FormFailure e -> Just e
    case res of
        FormMissing -> defaultLayout $ defaultWidget widget enctype mayError
        FormFailure _ -> defaultLayout $ defaultWidget widget enctype mayError
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

defaultWidget :: Widget -> Enctype -> Maybe [Text] -> Widget
defaultWidget widget enctype mayError = [whamlet|
    $maybe error <- mayError
        $forall e <- error
            <div .error .text-muted>#{e}
    <form method=post enctype=#{enctype}>
        ^{widget}
        <button type=submit>К оплате!
    |]
{-# LANGUAGE OverloadedStrings #-}
module Handler.Profile.Deposit where


import Import
import Form.Profile.Deposit


getDepositR :: Handler Html
getDepositR = do
    (widget, enctype) <- generateFormPost depositForm
    defaultLayout $ [whamlet|
        <form method=post enctype=#{enctype}>
            ^{widget}
            <button type=submit>К оплате!
        |]


postDepositR :: Handler Html
postDepositR = do
    ((res, widget), enctype) <- runFormPost depositForm
    mayError <- return $ case res of
        FormSuccess depReq -> Nothing
        FormMissing -> Just  ["No data"]
        FormFailure e -> Just e
    defaultLayout $ [whamlet|
        $maybe error <- mayError
            $forall e <- error
                <div .error .text-muted>#{e}
        <form method=post enctype=#{enctype}>
            ^{widget}
            <button type=submit>К оплате!
        |]
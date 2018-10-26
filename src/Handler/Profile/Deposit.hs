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
postDepositR = getDepositR
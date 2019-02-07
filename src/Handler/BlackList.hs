{-# LANGUAGE QuasiQuotes #-}
module Handler.BlackList ( getBlackListR ) where

import Import


getBlackListR :: Handler Html
getBlackListR = defaultLayout $ do
    setTitleI MsgBlackList
    [whamlet|
        <h5>_{MsgPrizmBlackListTitle}
        |]
{-# LANGUAGE TemplateHaskell #-}
module Handler.Operator.Bids where


import Import


getOperatorBidsR :: Handler Html
getOperatorBidsR = do
    defaultLayout $(widgetFile "operator/bids")
{-# LANGUAGE OverloadedStrings #-}
module Stocks.Widgets where

import           Import

import           Utils.Money           ( centsToCoins, dbl2MoneyT,
                                         fixedDoubleT )

import           Data.Maybe            ( fromJust )
import           Data.Time.Clock       ( diffUTCTime )
import           Data.Time.Clock.POSIX ( utcTimeToPOSIXSeconds )
import           Text.Julius           ( rawJS )


purchaseStatusW :: Text -> StocksPurchase -> Stocks -> Widget
purchaseStatusW htmlId p s
    | isJust (stocksPurchaseAccepted p) = purchaseParaminingW
            htmlId p s (fromJust $ stocksPurchaseAccepted p)
    | isJust (stocksPurchaseCancelled p) =
        [whamlet|<small>_{MsgUserCancelled}|]
    | Import.isNothing (stocksPurchaseUserConfirmed p) =
        [whamlet|
            <small>
                _{MsgPurchaseStatusNotConfirmed}
                (_{MsgSeeDetails})|]
    | otherwise = [whamlet|<small>_{MsgProcessing}|]

purchaseParaminingW :: Text -> StocksPurchase -> Stocks -> UTCTime -> Widget
purchaseParaminingW htmlId p s t = do
    timeNow <- liftIO getCurrentTime
    let
        amountCoins         :: Double
        paraminingTime      :: Double
        amountCoins         = centsToCoins (stocksPurchaseAmount p * stocksPrice s) / 10
        amount              = dbl2MoneyT amountCoins
        stocks              = stocksAbbr s
        paraminingTime      = realToFrac $ (diffUTCTime timeNow t) * 1000
        paraminingTimestamp = rawJS . fixedDoubleT 12 . realToFrac $
            utcTimeToPOSIXSeconds t * 1000
        paraminingRate      = stocksParaRate stocks
        paraminingAmount    = paraminingTime * paraminingRate * amountCoins
        paramining          = fixedDoubleT 7 paraminingAmount
    $(widgetFile "client/stocks/purchases/paramining")
  where
    stocksParaRate s =
        (stocksParaMonthlyPercent s / 100) / (30 * 24 * 60 * 60 * 1000)

    stocksParaMonthlyPercent "FNXB" = 20
    stocksParaMonthlyPercent "FNXS" = 22
    stocksParaMonthlyPercent "FNXP" = 24
    stocksParaMonthlyPercent _      = 0

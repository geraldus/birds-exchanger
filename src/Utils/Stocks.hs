module Utils.Stocks where

import           Import.NoFoundation

import           Data.Maybe          ( fromJust )


purchaseSignificantDate :: StocksPurchase -> UTCTime
purchaseSignificantDate p
        | isJust (stocksPurchaseAccepted p) =
            fromJust $ stocksPurchaseAccepted p
        | isJust (stocksPurchaseCancelled p) =
            fromJust $ stocksPurchaseCancelled p
        | otherwise = stocksPurchaseCreated p

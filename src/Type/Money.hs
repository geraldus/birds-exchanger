module Type.Money (
    Money(..),
    Percent,
    percentToDouble,
    percentToJSON,
    mkPercent,
    oneCoinCents
) where

import           ClassyPrelude.Yesod

import           Local.Persist.Currency


data Money = Money Int Currency deriving Show

newtype Percent = Percent { percentToDouble :: Double }
    deriving (Show)

instance ToJSON Percent where
    toJSON = toJSON . percentToDouble

percentToJSON :: Percent -> Value
percentToJSON = toJSON . percentToDouble

mkPercent :: Int -> Percent
mkPercent n
    | n > -1 && n < 101 = Percent { percentToDouble = fromIntegral n }
    | otherwise = Percent 0

oneCoinCents :: Int
oneCoinCents = 100

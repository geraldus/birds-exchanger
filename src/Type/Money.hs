module Type.Money where

import           Local.Persist.Currency

import           ClassyPrelude.Yesod


data Money = Money Int Currency deriving Show


oneCoinCents :: Int
oneCoinCents = 100

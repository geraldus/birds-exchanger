module Type.Money where

import           ClassyPrelude.Yesod

import           Local.Persist.Currency


data Money = Money Int Currency deriving Show


oneCoinCents :: Int
oneCoinCents = 100
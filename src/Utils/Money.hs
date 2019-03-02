module Utils.Money
where

import           ClassyPrelude.Yesod

import           Local.Persist.Currency
import           Local.Persist.Exchange
import           Type.Fee
import           Type.Money
import           Utils.Form             ( currencyOptionListRaw,
                                          transferOptionsRaw )

import           Formatting             ( sformat )
import qualified Formatting.Formatters  as F

truncCoins2Cents :: Double -> Int
truncCoins2Cents x = truncate $ x * fromIntegral oneCoinCents

-- | Calculate fee
calcFeeCents :: Fee -> Int -> Int
calcFeeCents (Percent p) c = ceiling $ fromIntegral c * p / fromIntegral oneCoinCents
calcFeeCents (CentsFixed f) c = c - f


-- | Defines devisor and qoutient when specifying exchange ratio for
-- given pair.  E.g. for PZM -> RUR and RUR -> PZM exchange orders
-- ratio should always be specified as PZM/RUR pair.
defPairDir :: ExchangePair -> ExchangePair
defPairDir p = case p of
    ExchangePzmRur -> ExchangeRurPzm
    ExchangeRurPzm -> ExchangeRurPzm

flipPair :: ExchangePair -> ExchangePair
flipPair ExchangePzmRur = ExchangeRurPzm
flipPair ExchangeRurPzm = ExchangePzmRur

unPairCurrency :: ExchangePair -> (Currency, Currency)
unPairCurrency ExchangePzmRur = (pzmC, rurC)
unPairCurrency ExchangeRurPzm = (rurC, pzmC)

normalizeRatio :: ExchangePair -> ExchangePair -> Double -> Double
normalizeRatio p d r
    | p == d = 1 / r
    | otherwise = r

-- | Convert ratio according to desired normalized pair exchange direction
fromNormalizedRatio :: ExchangePair -> Double -> Double
fromNormalizedRatio p = normalizeRatio (defPairDir p) p

convertCents :: Double -> Int -> Int
convertCents r a =
    let x = truncate $ fromIntegral a * r * fromIntegral oneCoinCents :: Int
    in truncate (fromIntegral x / fromIntegral oneCoinCents :: Double)


cents2dblT :: Int -> Text
cents2dblT n = dbl2MoneyT (fromIntegral n / fromIntegral oneCoinCents)

dbl2MoneyT :: Double -> Text
dbl2MoneyT = sformat (F.fixed (2 :: Int))


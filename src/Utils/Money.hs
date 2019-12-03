module Utils.Money
where

import           ClassyPrelude.Yesod

import           Local.Persist.Currency
import           Local.Persist.Exchange
import qualified Type.Fee as T (Fee(..))
import           Type.Money

import           Formatting             ( sformat )
import qualified Formatting.Formatters  as F


truncCoins2Cents :: Double -> Int
truncCoins2Cents x = truncate $ x * fromIntegral oneCoinCents

-- | Calculate fee
calcFeeCents :: T.Fee -> Int -> Int
calcFeeCents (T.Percent p) c = ceiling $ fromIntegral c * p / fromIntegral oneCoinCents
calcFeeCents (T.CentsFixed f) c = c - f


-- | Defines devisor and qoutient when specifying exchange ratio for
-- given pair.  E.g. for PZM -> RUR and RUR -> PZM exchange orders
-- ratio should always be specified as PZM/RUR pair.
defPairDir :: ExchangePair -> ExchangePair
defPairDir p = case p of
    ExchangePzmRur -> ExchangeRurPzm
    ExchangeRurPzm -> ExchangeRurPzm
    ExchangeRurOur -> ExchangeRurOur
    ExchangeOurRur -> ExchangeRurOur
    ExchangeOurPzm -> ExchangePzmOur
    ExchangePzmOur -> ExchangePzmOur

flipPair :: ExchangePair -> ExchangePair
flipPair ExchangePzmRur = ExchangeRurPzm
flipPair ExchangeRurPzm = ExchangePzmRur
flipPair ExchangeOurRur = ExchangeRurOur
flipPair ExchangeRurOur = ExchangeOurRur
flipPair ExchangePzmOur = ExchangeOurPzm
flipPair ExchangeOurPzm = ExchangePzmOur


unPairCurrency :: ExchangePair -> (Currency, Currency)
unPairCurrency ExchangePzmRur = (pzmC, rurC)
unPairCurrency ExchangeRurPzm = (rurC, pzmC)
unPairCurrency ExchangePzmOur = (pzmC, ourC)
unPairCurrency ExchangeOurPzm = (ourC, pzmC)
unPairCurrency ExchangeOurRur = (ourC, rurC)
unPairCurrency ExchangeRurOur = (rurC, ourC)

-- | Normalize ratio @r@ to preffered direction @d@ for given
--   exchange pair @p@
normalizeRatio :: ExchangePair -> ExchangePair -> Double -> Double
normalizeRatio p d r
    | p == d = 1 / r
    | otherwise = r

-- | Returns correct multiplicator (ratio) for conversion first
--   pair's (outgoing) currency to target currency
pairRatioByNormalizedRatio :: ExchangePair -> Double -> Double
pairRatioByNormalizedRatio p = normalizeRatio (defPairDir p) p

-- | Multiply integral amount cents @a@ on ratio @r@ returning
--   integral result.
multiplyCents :: Double -> Int -> Int
multiplyCents r a =
    let x = truncate $ fromIntegral a * r * fromIntegral oneCoinCents :: Int
    in truncate (fromIntegral x / fromIntegral oneCoinCents :: Double)

centsToCoins :: Int -> Double
centsToCoins n = fromIntegral n / fromIntegral oneCoinCents

cents2dblT :: Int -> Text
cents2dblT = dbl2MoneyT . centsToCoins

-- | Render 'Double' as 'Text' with two fractional digits
dbl2MoneyT :: Double -> Text
dbl2MoneyT = fixedDoubleT 2

-- | Render 'Double' as 'Text' with 'n' fractional digits
fixedDoubleT :: Int -> Double -> Text
fixedDoubleT = sformat . F.fixed

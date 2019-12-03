{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Utils.Render
    ( renderFeeAsDbl
    , renderFeeAsPct
    , renderCurrencyAmount
    , renderCurrencyAmount'
    )
where


import           Import.NoFoundation
import           Local.Persist.Currency ( Currency, currencyCodeT',
                                          currencySymbol )
import           Type.Fee               ( Fee (..) )
import           Type.Money             ( oneCoinCents )
import           Utils.Money            ( centsToCoins, fixedDoubleT )

import           Data.Time.Format       ( TimeLocale (..) )


renderFeeAsPct :: Fee -> Html
renderFeeAsPct (CentsFixed _) = error "no fixed fee logics"
renderFeeAsPct (Percent    p) = [shamlet|#{show p}|]

renderFeeAsDbl :: Fee -> Html
renderFeeAsDbl (CentsFixed _) = error "no fixed fee logics"
renderFeeAsDbl (Percent p) = [shamlet|#{show $ p / fromIntegral oneCoinCents}|]

renderCurrencyAmount ::
        TimeLocale -> [Text] -> [Text] -> Bool -> Currency -> Int -> Html
renderCurrencyAmount _loc currencyClasses' valueClasses' forceSignRender c n = [shamlet|
    <span class=#{valueClasses}>
        #{sign}#{amount}
    <small class=#{currencyClasses}>
        #{symbol}
    |]
  where
    amount = fixedDoubleT 2 (centsToCoins n)

    symbol = currencySymbol c

    code = currencyCodeT' c

    valueClasses
        | null valueClasses' =
                unwordsT ["value", code]
        | otherwise = unwordsT valueClasses'

    currencyClasses
        | null currencyClasses' =
                unwordsT ["currency-symbol", "text-muted", code]
        | otherwise = unwordsT currencyClasses'

    sign
        | forceSignRender && n > 0 = "+" :: Html
        | otherwise = ""

    unwordsT = intercalate " "

renderCurrencyAmount' :: Currency -> Int -> Html
renderCurrencyAmount' = renderCurrencyAmount (error "no locale") [] [] False

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Utils.Render
    ( renderFeeAsDbl
    , renderFeeAsPct
    , renderCurrencyAmount
    )
where


import           Import.NoFoundation
import           Local.Persist.Currency ( Currency, currSign, currencyCodeT',
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
        TimeLocale -> [Text] -> [Text] -> Currency -> Int -> Html
renderCurrencyAmount _loc currencyClasses' valueClasses' c n = [shamlet|
    <span class=#{valueClasses}>
        #{amount}
    <small class=#{currencyClasses}>
        #{symbol}
    |]
  where
    amount = fixedDoubleT 2 (centsToCoins n)

    symbol = currencySymbol c

    code = currencyCodeT' c

    valueClasses
        | null valueClasses' = intercalate
            " "
            ["value", "font-weight-bold", "text-primary", code]
        | otherwise = intercalate " " valueClasses'

    currencyClasses
        | null currencyClasses' = intercalate
            " "
            ["currency-symbol", "text-muted", code]
        | otherwise = intercalate " " currencyClasses'

{-# LANGUAGE QuasiQuotes #-}
module Utils.Render
    ( renderFeeAsDbl
    , renderFeeAsPct
    )
where


import           Import.NoFoundation
import           Type.Fee                       ( Fee(..) )
import           Type.Money                     ( oneCoinCents )


renderFeeAsPct :: Fee -> Html
renderFeeAsPct (CentsFixed _) = error "no fixed fee logics"
renderFeeAsPct (Percent    p) = [shamlet|#{show p}|]

renderFeeAsDbl :: Fee -> Html
renderFeeAsDbl (CentsFixed _) = error "no fixed fee logics"
renderFeeAsDbl (Percent p) = [shamlet|#{show $ p / fromIntegral oneCoinCents}|]

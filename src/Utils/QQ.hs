module Utils.QQ where

import           Language.Haskell.TH.Quote ( quoteFile, QuasiQuoter )
import           Text.Shakespeare.Text     ( st )


stFile :: QuasiQuoter
stFile = quoteFile st

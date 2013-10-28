{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Properties (tests) where

import Control.DeepSeq (NFData(..))
import Data.Text (Text)
import QuickCheckUtils ()
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Data.Text as T
import qualified Data.Text.ICU as I

t_rnf :: (NFData a) => (Text -> a) -> Text -> Bool
t_rnf f t = rnf (f t) == ()

t_nonEmpty :: (Text -> Text) -> Text -> Bool
t_nonEmpty f t
    | T.null t  = T.null ft
    | otherwise = T.length ft > 0
  where ft = f t

t_toCaseFold bool = t_nonEmpty $ I.toCaseFold bool
t_toLower locale = t_nonEmpty $ I.toLower locale
t_toUpper locale = t_nonEmpty $ I.toUpper locale

tests :: Test
tests =
  testGroup "Properties" [
    testProperty "t_toCaseFold" t_toCaseFold
  , testProperty "t_toLower" t_toLower
  , testProperty "t_toUpper" t_toUpper
  ]

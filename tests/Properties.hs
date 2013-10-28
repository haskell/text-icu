{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Properties (tests) where

import Control.DeepSeq (NFData(..))
import Data.Text (Text)
import QuickCheckUtils ()
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Data.Text.ICU as I

t_rnf :: (NFData a) => (Text -> a) -> Text -> Bool
t_rnf f t = rnf (f t) == ()

t_toCaseFold bool = t_rnf $ I.toCaseFold bool

tests :: Test
tests =
  testGroup "Properties" [
   testProperty "t_toCaseFold" t_toCaseFold
  ]

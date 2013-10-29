{-# OPTIONS_GHC -fno-warn-orphans #-}

module QuickCheckUtils () where

import Control.Applicative ((<$>))
import Control.DeepSeq (NFData(..))
import Data.Text.ICU (Collator, LocaleName(..), NormalizationMode(..))
import Data.Text.ICU.Break (available)
import Test.QuickCheck (Arbitrary(..), elements)
import qualified Data.Text as T
import qualified Data.Text.ICU as I

instance NFData Ordering where
    rnf v  = v `seq` ()

instance Arbitrary T.Text where
    arbitrary = T.pack `fmap` arbitrary
    shrink = map T.pack . shrink . T.unpack

instance Arbitrary LocaleName where
    arbitrary = elements (Root:available)

instance Arbitrary NormalizationMode where
    arbitrary = elements [None ..FCD]

instance Arbitrary Collator where
    arbitrary = I.collator <$> arbitrary

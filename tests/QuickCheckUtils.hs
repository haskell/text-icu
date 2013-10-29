{-# OPTIONS_GHC -fno-warn-orphans #-}

module QuickCheckUtils () where

import Data.Text.ICU (LocaleName(..), NormalizationMode(..))
import Data.Text.ICU.Break (available)
import Test.QuickCheck (Arbitrary(..), elements)
import qualified Data.Text as T

instance Arbitrary T.Text where
    arbitrary = T.pack `fmap` arbitrary
    shrink = map T.pack . shrink . T.unpack

instance Arbitrary LocaleName where
    arbitrary = elements (Root:available)

instance Arbitrary NormalizationMode where
    arbitrary = elements [None ..FCD]

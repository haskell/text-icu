{-# OPTIONS_GHC -fno-warn-orphans #-}

module QuickCheckUtils () where

import Data.Text.ICU (LocaleName(..))
import Test.QuickCheck (Arbitrary(..), elements)
import qualified Data.Text as T
import Data.Text.ICU.Break (available)

instance Arbitrary T.Text where
    arbitrary = T.pack `fmap` arbitrary
    shrink = map T.pack . shrink . T.unpack

instance Arbitrary LocaleName where
    arbitrary = elements (Root:available)

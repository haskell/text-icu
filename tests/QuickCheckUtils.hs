{-# OPTIONS_GHC -fno-warn-orphans #-}

module QuickCheckUtils () where

import Test.QuickCheck (Arbitrary(..))
import qualified Data.Text as T

instance Arbitrary T.Text where
    arbitrary = T.pack `fmap` arbitrary
    shrink = map T.pack . shrink . T.unpack

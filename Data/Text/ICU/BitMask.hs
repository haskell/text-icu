{-# LANGUAGE DefaultSignatures, ScopedTypeVariables #-}

-- From http://stackoverflow.com/a/15911213

module Data.Text.ICU.BitMask
       (
    -- * Bit mask twiddling API
    -- $api
    -- * Types
      ToBitMask
    -- * Functions
    , fromBitMask
    , highestValueInBitMask
    , toBitMask
    ) where


import Data.Bits ((.&.), (.|.))
import Data.Maybe (listToMaybe)

class ToBitMask a where
  toBitMask :: a -> Int

instance (ToBitMask a) => ToBitMask [a] where
  toBitMask = foldr ((.|.) . toBitMask) 0

fromBitMask :: (Enum a, Bounded a, ToBitMask a) => Int -> [a]
fromBitMask bm = filter inBitMask $ enumFrom minBound
  where inBitMask val = (bm .&. toBitMask val) == toBitMask val

highestValueInBitMask :: (Enum a, Bounded a, ToBitMask a) => Int -> Maybe a
highestValueInBitMask = listToMaybe . reverse . fromBitMask

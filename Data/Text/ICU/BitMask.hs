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
    , lowestValueInBitMask
    , toBitMask
    ) where


import Data.Bits ((.&.), (.|.), shiftL)
import Data.Maybe (listToMaybe)
import Control.Monad (msum, mzero)

class ToBitMask a where
  toBitMask :: a -> Int
  -- | Using a DefaultSignatures extension to declare a default signature with
  -- an `Enum` constraint without affecting the constraints of the class itself.
  default toBitMask :: Enum a => a -> Int
  toBitMask = shiftL 1 . fromEnum

instance ( ToBitMask a ) => ToBitMask [a] where
    toBitMask = foldr (.|.) 0 . map toBitMask

-- | Not making this a typeclass, since it already generalizes over all
-- imaginable instances with help of `MonadPlus`.
fromBitMask ::
    ( Enum a, Bounded a, ToBitMask a ) =>
        Int -> [a]
fromBitMask bm = msum $ map asInBM $ enumFrom minBound where
  asInBM a = if isInBitMask bm a then return a else mzero

lowestValueInBitMask ::
    ( Enum a, Bounded a, ToBitMask a ) =>
        Int -> Maybe a
lowestValueInBitMask = listToMaybe . fromBitMask

highestValueInBitMask ::
    ( Enum a, Bounded a, ToBitMask a ) =>
        Int -> Maybe a
highestValueInBitMask = listToMaybe . reverse . fromBitMask

isInBitMask :: ( ToBitMask a ) => Int -> a -> Bool
isInBitMask bm a = let aBM = toBitMask a in aBM == aBM .&. bm

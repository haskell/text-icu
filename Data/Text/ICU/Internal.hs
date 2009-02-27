module Data.Text.ICU.Internal
    (
      UBool
    , UChar
    , asBool
    , asOrdering
    ) where

import Data.Int (Int8)
import Data.Word (Word16)

type UBool = Int8
type UChar = Word16

asBool :: Integral a => a -> Bool
{-# INLINE asBool #-}
asBool = (/=0)

asOrdering :: Integral a => a -> Ordering
{-# INLINE asOrdering #-}
asOrdering i
    | i < 0     = LT
    | i == 0    = EQ
    | otherwise = GT

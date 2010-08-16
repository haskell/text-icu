module Data.Text.ICU.Internal
    (
      UBool
    , UChar
    , asBool
    , asOrdering
    , withName
    ) where

import Data.Int (Int8)
import Data.Word (Word16)
import Foreign.C.String (CString, withCString)
import Foreign.Ptr (nullPtr)

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

withName :: String -> (CString -> IO a) -> IO a
withName name act
    | null name = act nullPtr
    | otherwise = withCString name act

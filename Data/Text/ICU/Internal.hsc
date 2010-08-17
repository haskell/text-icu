{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface #-}

module Data.Text.ICU.Internal
    (
      UBool
    , UChar
    , UCharIterator
    , CharIterator(..)
    , asBool
    , asOrdering
    , withCharIterator
    , withName
    ) where

#include <unicode/uiter.h>

import Data.ByteString.Internal (ByteString(..))
import Data.Int (Int8, Int32)
import Data.Text (Text)
import Data.Text.Foreign (useAsPtr)
import Data.Word (Word16)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CChar)
import Foreign.Marshal.Alloc (alloca)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (Storable(..))

-- | A type that supports efficient iteration over Unicode characters.
--
-- As an example of where this may be useful, a function using this
-- type may be able to iterate over a UTF-8 'ByteString' directly,
-- rather than first copying and converting it to an intermediate
-- form.  This type also allows e.g. comparison between 'Text' and
-- 'ByteString', with minimal overhead.
data CharIterator = CIText !Text
                  | CIUTF8 !ByteString

instance Show CharIterator where
    show (CIText t)  = show t
    show (CIUTF8 bs) = show bs

data UCharIterator

instance Storable UCharIterator where
    sizeOf _    = #{size UCharIterator}
    alignment _ = alignment (undefined :: CString)

-- | Temporarily allocate a 'UCharIterator' and use it with the
-- contents of the to-be-iterated-over string.
withCharIterator :: CharIterator -> (Ptr UCharIterator -> IO a) -> IO a
withCharIterator (CIUTF8 (PS fp _ l)) act =
    alloca $ \i -> withForeignPtr fp $ \p ->
    uiter_setUTF8 i (castPtr p) (fromIntegral l) >> act i
withCharIterator (CIText t) act =
    alloca $ \i -> useAsPtr t $ \p l ->
    uiter_setString i p (fromIntegral l) >> act i

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

foreign import ccall unsafe "hs_text_icu.h __hs_uiter_setString" uiter_setString
    :: Ptr UCharIterator -> Ptr UChar -> Int32 -> IO ()

foreign import ccall unsafe "hs_text_icu.h __hs_uiter_setUTF8" uiter_setUTF8
    :: Ptr UCharIterator -> Ptr CChar -> Int32 -> IO ()

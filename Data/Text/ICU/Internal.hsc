{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface, GeneralizedNewtypeDeriving, TupleSections #-}

module Data.Text.ICU.Internal
    (
      LocaleName(..)
    , UBool
    , UChar
    , UChar32
    , UCharIterator
    , CharIterator(..)
    , UText, UTextPtr
    , asBool
    , asOrdering
    , withCharIterator
    , withLocaleName
    , withName
    , useAsUCharPtr, fromUCharPtr, I16, asUCharForeignPtr
    , asUTextPtr, withUTextPtr, withUTextPtrText, emptyUTextPtr, utextPtrLength
    , TextI, takeWord, dropWord, lengthWord
    ) where

#include <unicode/uiter.h>

import Control.Exception (mask_)
import Control.DeepSeq (NFData(..))
import Data.ByteString.Internal (ByteString(..))
import Data.Int (Int8, Int32, Int64)
import Data.String (IsString(..))
import Data.Text (Text, empty)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Foreign (useAsPtr, asForeignPtr, fromPtr)
#if MIN_VERSION_text(2,0,0)
import Data.Text.Foreign (I8, dropWord8, takeWord8, lengthWord8)
import Data.Word (Word8)
import Foreign.ForeignPtr (mallocForeignPtrArray)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Storable (peek)
#else
import Data.Text.Foreign (I16, dropWord16, takeWord16, lengthWord16)
#endif
import Data.Word (Word16, Word32)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CChar)
import Foreign.ForeignPtr (withForeignPtr, ForeignPtr, newForeignPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr, nullPtr, FunPtr)
import Data.Text.ICU.Error.Internal (UErrorCode)
import System.IO.Unsafe (unsafePerformIO)

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
    show (CIUTF8 bs) = show (decodeUtf8 bs)

data UCharIterator

-- | Temporarily allocate a 'UCharIterator' and use it with the
-- contents of the to-be-iterated-over string.
withCharIterator :: CharIterator -> (Ptr UCharIterator -> IO a) -> IO a
withCharIterator (CIUTF8 (PS fp _ l)) act =
    allocaBytes (#{size UCharIterator}) $ \i -> withForeignPtr fp $ \p ->
    uiter_setUTF8 i (castPtr p) (fromIntegral l) >> act i
withCharIterator (CIText t) act =
    allocaBytes (#{size UCharIterator}) $ \i -> useAsPtr t $ \p l ->
#if MIN_VERSION_text(2,0,0)
    uiter_setUTF8 i (castPtr p) (fromIntegral l) >> act i
#else
    uiter_setString i p (fromIntegral l) >> act i
#endif

type UBool   = Int8
type UChar   = Word16
type UChar32 = Word32

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

-- | The name of a locale.
data LocaleName = Root
                -- ^ The root locale.  For a description of resource bundles
                -- and the root resource, see
                -- <http://userguide.icu-project.org/locale/resources>.
                | Locale String -- ^ A specific locale.
                | Current       -- ^ The program's current locale.
                  deriving (Eq, Ord, Read, Show)

instance NFData LocaleName where
    rnf Root       = ()
    rnf (Locale l) = rnf l
    rnf Current    = ()

instance IsString LocaleName where
    fromString = Locale

withLocaleName :: LocaleName -> (CString -> IO a) -> IO a
withLocaleName Current act = act nullPtr
withLocaleName Root act = withCString "" act
withLocaleName (Locale n) act = withCString n act

#if !MIN_VERSION_text(2,0,0)
foreign import ccall unsafe "hs_text_icu.h __hs_uiter_setString" uiter_setString
    :: Ptr UCharIterator -> Ptr UChar -> Int32 -> IO ()
#endif

foreign import ccall unsafe "hs_text_icu.h __hs_uiter_setUTF8" uiter_setUTF8
    :: Ptr UCharIterator -> Ptr CChar -> Int32 -> IO ()


data UText

-- | Pointer to UText which also keeps pointer to source text so it won't be
-- garbage collected.
data UTextPtr
    = UTextPtr
      { utextPtr :: ForeignPtr UText
      , utextPtrText :: ForeignPtr TextChar
      , utextPtrLength :: TextI
      }

emptyUTextPtr :: UTextPtr
emptyUTextPtr = unsafePerformIO $ asUTextPtr empty
{-# NOINLINE emptyUTextPtr #-}

withUTextPtr :: UTextPtr -> (Ptr UText -> IO a) -> IO a
withUTextPtr = withForeignPtr . utextPtr

withUTextPtrText :: UTextPtr -> (Ptr TextChar -> IO a) -> IO a
withUTextPtrText = withForeignPtr . utextPtrText

-- | Returns UTF-8 UText for text >= 2.0 or UTF-16 UText for previous versions.
asUTextPtr :: Text -> IO UTextPtr
asUTextPtr t = do
    (fp,l) <- asForeignPtr t
    with 0 $ \ e -> withForeignPtr fp $ \ p -> mask_ $ do
        ut <- newForeignPtr utext_close =<<
#if MIN_VERSION_text(2,0,0)
            utext_openUTF8
#else
            utext_openUChars
#endif
            nullPtr p (fromIntegral l) e
        return $ UTextPtr ut fp l

foreign import ccall unsafe "hs_text_icu.h &__hs_utext_close" utext_close
    :: FunPtr (Ptr UText -> IO ())

useAsUCharPtr :: Text -> (Ptr UChar -> I16 -> IO a) -> IO a
asUCharForeignPtr :: Text -> IO (ForeignPtr UChar, I16)
fromUCharPtr :: Ptr UChar -> I16 -> IO Text

dropWord, takeWord :: TextI -> Text -> Text
lengthWord :: Text -> Int

#if MIN_VERSION_text(2,0,0)
newtype I16 = I16 Int
    deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

type TextChar = Word8
type TextI = I8

useAsUCharPtr t act = useAsPtr t $ \tptr tlen ->
    allocaArray (fromIntegral tlen) $ \ dst ->
        act dst =<< fromUtf8 dst tptr tlen

asUCharForeignPtr t = useAsPtr t $ \tptr tlen -> do
    fp <- mallocForeignPtrArray (fromIntegral tlen)
    withForeignPtr fp $ \ dst -> (fp,) <$> fromUtf8 dst tptr tlen

fromUtf8 :: Ptr UChar -> Ptr Word8 -> I8 -> IO I16
fromUtf8 dst tptr tlen =
    with 0 $ \ err ->
    with 0 $ \ dstLen -> do
        _ <- u_strFromUTF8Lenient dst (fromIntegral tlen) dstLen tptr
              (fromIntegral tlen) err
        fromIntegral <$> peek dstLen

fromUCharPtr p l =
    with 0 $ \ err ->
    with 0 $ \ dstLen ->
    allocaArray capacity $ \ dst -> do
        _ <- u_strToUTF8 dst (fromIntegral capacity) dstLen p
            (fromIntegral l) err
        dl <- peek dstLen
        fromPtr dst (fromIntegral dl)
    where capacity = fromIntegral l * 3

dropWord = dropWord8
takeWord = takeWord8
lengthWord = lengthWord8

foreign import ccall unsafe "hs_text_icu.h __hs_u_strFromUTF8Lenient" u_strFromUTF8Lenient
    :: Ptr UChar -> Int32 -> Ptr Int32 -> Ptr Word8 -> Int32 -> Ptr UErrorCode
    -> IO CString

foreign import ccall unsafe "hs_text_icu.h __hs_u_strToUTF8" u_strToUTF8
    :: Ptr Word8 -> Int32 -> Ptr Int32 -> Ptr UChar -> Int32 -> Ptr UErrorCode
    -> IO CString

foreign import ccall unsafe "hs_text_icu.h __hs_utext_openUTF8" utext_openUTF8
    :: Ptr UText -> Ptr Word8 -> Int64 -> Ptr UErrorCode -> IO (Ptr UText)

#else

type TextChar = UChar
type TextI = I16

-- text < 2.0 has UChar as internal representation.
useAsUCharPtr = useAsPtr
asUCharForeignPtr = asForeignPtr
fromUCharPtr = fromPtr

dropWord = dropWord16
takeWord = takeWord16
lengthWord = lengthWord16

foreign import ccall unsafe "hs_text_icu.h __hs_utext_openUChars" utext_openUChars
    :: Ptr UText -> Ptr UChar -> Int64 -> Ptr UErrorCode -> IO (Ptr UText)

#endif

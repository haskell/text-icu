{-# LANGUAGE CPP, EmptyDataDecls, ForeignFunctionInterface, DeriveDataTypeable #-}
module Data.Text.ICU.Converter
    (
    -- * Character set conversion
      Converter
    -- ** Basic functions
    , open
    , decode
    , encode
    -- ** Miscellaneous functions
    , getName
    -- * Global behavior
    , getDefaultName
    , setDefaultName
    -- * Miscellaneous functions
    , compareNames
    ) where

import Data.ByteString.Internal (ByteString, createAndTrim)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Text.Foreign (fromPtr, lengthWord16, useAsPtr)
import Data.Text.ICU.Error.Internal (UErrorCode, handleError)
import Data.Typeable (Typeable)
import Data.Word (Word16)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CInt)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr)
import System.IO.Unsafe (unsafePerformIO)

type UChar = Word16

data UConverter

-- | Character set converter type.  /Note/: this structure is not
-- thread safe. It is /not/ safe to use value of this type
-- simultaneously from multiple threads.
data Converter = Converter {-# UNPACK #-} !(ForeignPtr UConverter)
                 deriving (Eq, Typeable)

instance Show Converter where
    show c = "Converter " ++ show (getName c)

withConverter :: Converter -> (Ptr UConverter -> IO a) -> IO a
withConverter (Converter cnv) action = withForeignPtr cnv action

-- | Gets the internal, canonical name of the converter.
getName :: Converter -> String
getName cnv = unsafePerformIO .
  withConverter cnv $ \ptr ->
    peekCString =<< handleError (ucnv_getName ptr)

-- | Do a fuzzy compare of two converter/alias names.  The comparison
-- is case-insensitive, ignores leading zeroes if they are not
-- followed by further digits, and ignores all but letters and digits.
-- Thus the strings @\"UTF-8\"@, @\"utf_8\"@, @\"u*T\@f08\"@ and
-- @\"Utf 8\"@ are exactly equivalent.  See section 1.4, Charset Alias
-- Matching in Unicode Technical Standard #22 at
-- <http://www.unicode.org/reports/tr22/>
compareNames :: String -> String -> Ordering
compareNames a b =
  unsafePerformIO . withCString a $ \aptr ->
    withCString b $ return . ord . ucnv_compareNames aptr
  where ord i | i < 0     = LT
              | i == 0    = EQ
              | otherwise = GT

-- | Create a 'Converter' with the name of a coded character set
-- specified as a string.  The actual name will be resolved with the
-- alias file using a case-insensitive string comparison that ignores
-- leading zeroes and all non-alphanumeric characters.  E.g., the
-- names @\"UTF8\"@, @\"utf-8\"@, @\"u*T\@f08\"@ and @\"Utf 8\"@ are
-- all equivalent (see also 'compareNames').  If an empty string is
-- passed for the converter name, it will create one with the
-- 'getDefaultName' return value.
--
-- A converter name may contain options like a locale specification to
-- control the specific behavior of the newly instantiated converter.
-- The meaning of the options depends on the particular converter.  If
-- an option is not defined for or recognized by a given converter,
-- then it is ignored.
--
-- Options are appended to the converter name string, with a comma
-- between the name and the first option and also between adjacent
-- options.
--
-- If the alias is ambiguous, then the preferred converter is used.
--
-- The conversion behavior and names can vary between platforms. ICU
-- may convert some characters differently from other
-- platforms. Details on this topic are in the ICU User's Guide at
-- <http://icu-project.org/userguide/conversion.html>. Aliases
-- starting with a @\"cp\"@ prefix have no specific meaning other than
-- its an alias starting with the letters @\"cp\"@. Please do not
-- associate any meaning to these aliases.
open :: String -> IO Converter
open name =
  fmap Converter . newForeignPtr ucnv_close =<< named (handleError . ucnv_open)
  where named act
            | null name = act nullPtr
            | otherwise = withCString name act

-- | Convert the Unicode string into a codepage string using the given
-- converter.
encode :: Converter -> Text -> IO ByteString
encode cnv t =
  useAsPtr t $ \tptr tlen ->
    withConverter cnv $ \cptr -> do
      let capacity = fromIntegral . max_bytes_for_string cptr . fromIntegral $
                     lengthWord16 t
      createAndTrim (fromIntegral capacity) $ \sptr ->
        fmap fromIntegral . handleError $
           ucnv_fromUChars cptr (castPtr sptr) capacity tptr (fromIntegral tlen)

-- | Convert the codepage string into a Unicode string using the given
-- converter.
decode :: Converter -> ByteString -> IO Text
decode cnv bs =
  unsafeUseAsCStringLen bs $ \(sptr, slen) ->
    withConverter cnv $ \cptr -> do
      let capacity = slen * 2
      allocaArray capacity $ \tptr -> 
        fromPtr tptr =<< (fmap fromIntegral . handleError $
                          ucnv_toUChars cptr tptr (fromIntegral capacity) sptr
                                        (fromIntegral slen))

-- | Returns the current default converter name. If you want to 'open'
-- a default converter, you do not need to use this function.  It is
-- faster to pass the empty string to 'open' the default converter.
getDefaultName :: IO String
getDefaultName = peekCString =<< ucnv_getDefaultName

-- | Sets the current default converter name. If this function needs
-- to be called, it should be called during application
-- initialization. Most of the time, the results from 'getDefaultName'
-- or 'open' with an empty string argument is sufficient for your
-- application.
--
-- /Note/: this function is not thread safe. /Do not/ call this
-- function when /any/ ICU function is being used from more than one
-- thread!
setDefaultName :: String -> IO ()
setDefaultName s = withCString s $ ucnv_setDefaultName

foreign import ccall unsafe "unicode/ucnv.h ucnv_open_4_0" ucnv_open
    :: CString -> Ptr UErrorCode -> IO (Ptr UConverter)

foreign import ccall unsafe "unicode/ucnv.h ucnv_getName_4_0" ucnv_getName
    :: Ptr UConverter -> Ptr UErrorCode -> IO CString

foreign import ccall unsafe "unicode/ucnv.h &ucnv_close_4_0" ucnv_close
    :: FunPtr (Ptr UConverter -> IO ())

foreign import ccall unsafe "__get_max_bytes_for_string" max_bytes_for_string
    :: Ptr UConverter -> CInt -> CInt

foreign import ccall unsafe "unicode/ucnv.h ucnv_toUChars_4_0" ucnv_toUChars
    :: Ptr UConverter -> Ptr UChar -> Int32 -> CString -> Int32
    -> Ptr UErrorCode -> IO Int32

foreign import ccall unsafe "unicode/ucnv.h ucnv_fromUChars_4_0" ucnv_fromUChars
    :: Ptr UConverter -> CString -> Int32 -> Ptr UChar -> Int32
    -> Ptr UErrorCode -> IO Int32

foreign import ccall unsafe "unicode/ucnv.h ucnv_compareNames_4_0" ucnv_compareNames
    :: CString -> CString -> CInt

foreign import ccall unsafe "unicode/ucnv.h ucnv_getDefaultName_4_0" ucnv_getDefaultName
    :: IO CString

foreign import ccall unsafe "unicode/ucnv.h ucnv_setDefaultName_4_0" ucnv_setDefaultName
    :: CString -> IO ()

foreign import ccall unsafe "unicode/ucnv.h ucnv_reset_4_0" ucnv_reset
    :: IO ()

{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : Data.Text.ICU.Convert
-- Copyright   : (c) 2009, 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Character set conversion functions for Unicode, implemented as
-- bindings to the International Components for Unicode (ICU)
-- libraries.
module Data.Text.ICU.Convert
    (
    -- * Character set conversion
      Converter
    -- ** Basic functions
    , open
    , fromUnicode
    , toUnicode
    -- ** Converter metadata
    , getName
    , usesFallback
    , isAmbiguous
    -- * Functions for controlling global behavior
    , getDefaultName
    , setDefaultName
    -- * Miscellaneous functions
    , compareNames
    , aliases
    -- * Metadata
    , converterNames
    , standardNames
    ) where

import Data.ByteString.Internal (ByteString, createAndTrim)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Text.Foreign (fromPtr, lengthWord16, useAsPtr)
import Data.Text.ICU.Convert.Internal
import Data.Text.ICU.Error.Internal (UErrorCode, handleError)
import Data.Word (Word16)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CInt(..))
import Foreign.ForeignPtr (newForeignPtr)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (FunPtr, Ptr, castPtr)
import System.IO.Unsafe (unsafePerformIO)
import Data.Text.ICU.Internal (UBool, UChar, asBool, asOrdering, withName)

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
    fmap asOrdering . withCString b $ ucnv_compareNames aptr

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
open :: String                  -- ^ Name of the converter to use.
     -> Maybe Bool              -- ^ Whether to use fallback mappings
                                -- (see 'usesFallback' for details).
     -> IO Converter
open name mf = do
  c <- fmap Converter . newForeignPtr ucnv_close =<< withName name (handleError . ucnv_open)
  case mf of
    Just f -> withConverter c $ \p -> ucnv_setFallback p . fromIntegral . fromEnum $ f
    _ -> return ()
  return c

-- | Convert the Unicode string into a codepage string using the given
-- converter.
fromUnicode :: Converter -> Text -> ByteString
fromUnicode cnv t =
  unsafePerformIO . useAsPtr t $ \tptr tlen ->
    withConverter cnv $ \cptr -> do
      let capacity = fromIntegral . max_bytes_for_string cptr . fromIntegral $
                     lengthWord16 t
      createAndTrim (fromIntegral capacity) $ \sptr ->
        fmap fromIntegral . handleError $
           ucnv_fromUChars cptr (castPtr sptr) capacity tptr (fromIntegral tlen)

-- | Convert the codepage string into a Unicode string using the given
-- converter.
toUnicode :: Converter -> ByteString -> Text
toUnicode cnv bs =
  unsafePerformIO . unsafeUseAsCStringLen bs $ \(sptr, slen) ->
    withConverter cnv $ \cptr -> do
      let capacity = slen * 2
      allocaArray capacity $ \tptr ->
        fromPtr tptr =<< (fmap fromIntegral . handleError $
                          ucnv_toUChars cptr tptr (fromIntegral capacity) sptr
                                        (fromIntegral slen))

-- | Determines whether the converter uses fallback mappings or not.
-- This flag has restrictions.  Regardless of this flag, the converter
-- will always use fallbacks from Unicode Private Use code points, as
-- well as reverse fallbacks (to Unicode).  For details see \".ucm
-- File Format\" in the Conversion Data chapter of the ICU User Guide:
-- <http://www.icu-project.org/userguide/conversion-data.html#ucmformat>
usesFallback :: Converter -> Bool
usesFallback cnv = unsafePerformIO $
                   asBool `fmap` withConverter cnv ucnv_usesFallback

-- | Returns the current default converter name. If you want to 'open'
-- a default converter, you do not need to use this function.  It is
-- faster to pass the empty string to 'open' the default converter.
getDefaultName :: IO String
getDefaultName = peekCString =<< ucnv_getDefaultName

-- | Indicates whether the converter contains ambiguous mappings of
-- the same character or not.
isAmbiguous :: Converter -> Bool
isAmbiguous cnv = asBool . unsafePerformIO $ withConverter cnv ucnv_isAmbiguous

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

-- | A list of the canonical names of all available converters.
converterNames :: [String]
{-# NOINLINE converterNames #-}
converterNames = unsafePerformIO $
  mapM ((peekCString =<<) . ucnv_getAvailableName) [0..ucnv_countAvailable-1]

-- | The list of supported standard names.
standardNames :: [String]
{-# NOINLINE standardNames #-}
standardNames = filter (not . null) . unsafePerformIO $
  mapM ((peekCString =<<) . handleError . ucnv_getStandard) [0..ucnv_countStandards-1]

-- | Return the aliases for a given converter or alias name.
aliases :: String -> [String]
aliases name = unsafePerformIO . withCString name $ \ptr -> do
  count <- handleError $ ucnv_countAliases ptr
  if count == 0
    then return []
    else mapM ((peekCString =<<) . handleError . ucnv_getAlias ptr) [0..count-1]

foreign import ccall unsafe "hs_text_icu.h __hs_ucnv_open" ucnv_open
    :: CString -> Ptr UErrorCode -> IO (Ptr UConverter)

foreign import ccall unsafe "hs_text_icu.h &__hs_ucnv_close" ucnv_close
    :: FunPtr (Ptr UConverter -> IO ())

foreign import ccall unsafe "__get_max_bytes_for_string" max_bytes_for_string
    :: Ptr UConverter -> CInt -> CInt

foreign import ccall unsafe "hs_text_icu.h __hs_ucnv_toUChars" ucnv_toUChars
    :: Ptr UConverter -> Ptr UChar -> Int32 -> CString -> Int32
    -> Ptr UErrorCode -> IO Int32

foreign import ccall unsafe "hs_text_icu.h __hs_ucnv_fromUChars" ucnv_fromUChars
    :: Ptr UConverter -> CString -> Int32 -> Ptr UChar -> Int32
    -> Ptr UErrorCode -> IO Int32

foreign import ccall unsafe "hs_text_icu.h __hs_ucnv_compareNames" ucnv_compareNames
    :: CString -> CString -> IO CInt

foreign import ccall unsafe "hs_text_icu.h __hs_ucnv_getDefaultName" ucnv_getDefaultName
    :: IO CString

foreign import ccall unsafe "hs_text_icu.h __hs_ucnv_setDefaultName" ucnv_setDefaultName
    :: CString -> IO ()

foreign import ccall unsafe "hs_text_icu.h __hs_ucnv_countAvailable" ucnv_countAvailable
    :: Int32

foreign import ccall unsafe "hs_text_icu.h __hs_ucnv_getAvailableName" ucnv_getAvailableName
    :: Int32 -> IO CString

foreign import ccall unsafe "hs_text_icu.h __hs_ucnv_countAliases" ucnv_countAliases
    :: CString -> Ptr UErrorCode -> IO Word16

foreign import ccall unsafe "hs_text_icu.h __hs_ucnv_getAlias" ucnv_getAlias
    :: CString -> Word16 -> Ptr UErrorCode -> IO CString

foreign import ccall unsafe "hs_text_icu.h __hs_ucnv_countStandards" ucnv_countStandards
    :: Word16

foreign import ccall unsafe "hs_text_icu.h __hs_ucnv_getStandard" ucnv_getStandard
    :: Word16 -> Ptr UErrorCode -> IO CString

foreign import ccall unsafe "hs_text_icu.h __hs_ucnv_usesFallback" ucnv_usesFallback
    :: Ptr UConverter -> IO UBool

foreign import ccall unsafe "hs_text_icu.h __hs_ucnv_setFallback" ucnv_setFallback
    :: Ptr UConverter -> UBool -> IO ()

foreign import ccall unsafe "hs_text_icu.h __hs_ucnv_isAmbiguous" ucnv_isAmbiguous
    :: Ptr UConverter -> IO UBool

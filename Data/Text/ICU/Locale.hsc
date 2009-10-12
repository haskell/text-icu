{-# LANGUAGE CPP, DeriveDataTypeable, ForeignFunctionInterface,
    GeneralizedNewtypeDeriving #-}
module Data.Text.ICU.Locale
    (
      available
    , getDefault
    , setDefault
    , getName
    )
    where

#include <unicode/uloc.h>

import Control.Exception (try)
import Data.Int (Int32)
import Data.Text.ICU.Error.Codes (ErrorCode, UErrorCode)
import Data.Text.ICU.Error.Internal (handleError, preflight)
import Foreign.C.String (CString, peekCAString, withCAString)
import Foreign.Ptr (Ptr)
import System.IO.Unsafe (unsafePerformIO)

type Locale = String

-- | A list of the available locales,
-- e.g. @["af","af_NA","af_ZA","am","am_ET",...]@.
available :: [Locale]
available = unsafePerformIO $ do
  n <- countAvailable
  mapM getAvailable [0..n-1]

getAvailable :: (Integral a) => a -> IO Locale
getAvailable n = peekCAString =<< uloc_getAvailable (fromIntegral n)

countAvailable :: IO Int
countAvailable = fromIntegral `fmap` uloc_countAvailable

getDefault :: IO Locale
getDefault = peekCAString =<< uloc_getDefault

setDefault :: Locale -> IO ()
setDefault locale = withCAString locale (handleError . uloc_setDefault)

-- | Get the full name for the specified locale.
--
-- Note: This has the effect of \"canonicalizing\" the ICU locale ID
-- to a certain extent. Upper and lower case are set as needed. It
-- does /not/ map aliased names in any way.
getName :: String -> Either ErrorCode Locale
getName locale = unsafePerformIO . withCAString locale $
                 (try . preflight 16 . uloc_getName)

foreign import ccall unsafe "unicode/uloc.h uloc_getAvailable_4_0" uloc_getAvailable
    :: Int32 -> IO CString
foreign import ccall unsafe "unicode/uloc.h uloc_countAvailable_4_0" uloc_countAvailable
    :: IO Int32
foreign import ccall unsafe "unicode/uloc.h uloc_getDefault_4_0" uloc_getDefault
    :: IO CString
foreign import ccall unsafe "unicode/uloc.h uloc_setDefault_4_0" uloc_setDefault
    :: CString -> Ptr UErrorCode -> IO ()
foreign import ccall unsafe "unicode/uloc.h uloc_getName_4_0" uloc_getName
    :: CString -> CString -> Int32 -> Ptr UErrorCode -> IO Int32

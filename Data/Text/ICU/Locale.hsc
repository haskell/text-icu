{-# LANGUAGE CPP, DeriveDataTypeable, ForeignFunctionInterface,GeneralizedNewtypeDeriving #-}
module Data.Text.ICU.Locale
    (availableLocales,getDefaultLocale,setDefaultLocale,getLocaleName)
where

#include <unicode/uloc.h>

import Data.Int (Int32)
import Data.Text.ICU.Error.Internal (ErrorCode,UErrorCode,isSuccess,withError)
import Foreign.C.String (CString,peekCAString,peekCAStringLen,withCAString)
import Foreign.Marshal.Array (allocaArray0)
import Foreign.Ptr (Ptr)
import System.IO.Unsafe (unsafePerformIO)

-- |Return a list of the available locales, e.g. @["af","af_NA","af_ZA","am","am_ET",...]@.
availableLocales :: [String]
availableLocales = unsafePerformIO $ do
  n <- countAvailable
  sequence [getAvailable i | i<-[0..n-1]]

getAvailable :: (Integral a) => a -> IO String
getAvailable n = do
  cs <- uloc_getAvailable (fromIntegral n)
  peekCAString cs

countAvailable :: IO Int
countAvailable = do
  n <- uloc_countAvailable
  return (fromIntegral n)

getDefaultLocale :: IO String
getDefaultLocale = do
  cs <- uloc_getDefault
  peekCAString cs

setDefaultLocale :: String -> IO (Maybe ErrorCode)
setDefaultLocale locale = do
  withCAString locale $ \locale' -> do
    (err,_) <- withError $ uloc_setDefault locale'
    if isSuccess err
       then return Nothing
       else return (Just err)

getLocaleName :: String -> Either ErrorCode String
getLocaleName locale = unsafePerformIO $ do
  withCAString locale $ \locale' -> do
    allocaArray0 256 $ \name -> do
      (err,l) <- withError $ uloc_getName locale' name 256
      if isSuccess err
         then do
           name' <- peekCAStringLen (name,fromIntegral l)
           return (Right name')
         else return (Left err)

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

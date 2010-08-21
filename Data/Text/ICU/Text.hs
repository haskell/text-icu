{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : Data.Text.ICU.Text
-- Copyright   : (c) 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Functions for manipulating Unicode text, implemented as bindings to
-- the International Components for Unicode (ICU) libraries.
module Data.Text.ICU.Text
    (
      toLower
    , toUpper
    ) where

import Data.Int (Int32)
import Data.Text (Text)
import Data.Text.Foreign (fromPtr, useAsPtr)
import Data.Text.ICU.Error.Internal (UErrorCode, handleError)
import Data.Text.ICU.Internal (LocaleName, UChar, withLocaleName)
import Foreign.C.String (CString)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr)
import System.IO.Unsafe (unsafePerformIO)

toLower :: LocaleName -> Text -> Text
toLower loc s = unsafePerformIO .
  withLocaleName loc $ \locale ->
    useAsPtr s $ \sptr slen -> do
      let go len = allocaArray len $ \dptr -> do
            n <- fmap fromIntegral . handleError $
                 u_strToLower dptr (fromIntegral len) sptr
                              (fromIntegral slen) locale
            if n > len
              then go n
              else fromPtr dptr n
      go slen

toUpper :: LocaleName -> Text -> Text
toUpper loc s = unsafePerformIO .
  withLocaleName loc $ \locale ->
    useAsPtr s $ \sptr slen -> do
      let go len = allocaArray len $ \dptr -> do
            n <- fmap fromIntegral . handleError $
                 u_strToUpper dptr (fromIntegral len) sptr
                              (fromIntegral slen) locale
            if n > len
              then go n
              else fromPtr dptr n
      go slen

foreign import ccall unsafe "hs_text_icu.h __hs_u_strToLower" u_strToLower
    :: Ptr UChar -> Int32 -> Ptr UChar -> Int32 -> CString -> Ptr UErrorCode
    -> IO Int32

foreign import ccall unsafe "hs_text_icu.h __hs_u_strToUpper" u_strToUpper
    :: Ptr UChar -> Int32 -> Ptr UChar -> Int32 -> CString -> Ptr UErrorCode
    -> IO Int32

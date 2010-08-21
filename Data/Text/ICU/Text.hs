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
    -- * Case conversion
      toCaseFold
    , toLower
    , toUpper
    ) where

import Data.Int (Int32)
import Data.Text (Text)
import Data.Text.Foreign (fromPtr, useAsPtr)
import Data.Text.ICU.Error.Internal (UErrorCode, handleError)
import Data.Text.ICU.Internal (LocaleName, UChar, withLocaleName)
import Data.Word (Word32)
import Foreign.C.String (CString)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr)
import System.IO.Unsafe (unsafePerformIO)

toCaseFold :: Bool -> Text -> Text
toCaseFold excludeI s = unsafePerformIO .
  useAsPtr s $ \sptr slen -> do
    let opts = fromIntegral . fromEnum $ excludeI
        go len = allocaArray len $ \dptr -> do
          n <- fmap fromIntegral . handleError $
               u_strFoldCase dptr (fromIntegral len) sptr
                                  (fromIntegral slen) opts
          if n > len
            then go n
            else fromPtr dptr n
    go slen

type CaseMapper = Ptr UChar -> Int32 -> Ptr UChar -> Int32 -> CString
                -> Ptr UErrorCode -> IO Int32

caseMap :: CaseMapper -> LocaleName -> Text -> Text
caseMap mapFn loc s = unsafePerformIO .
  withLocaleName loc $ \locale ->
    useAsPtr s $ \sptr slen -> do
      let go len = allocaArray len $ \dptr -> do
            n <- fmap fromIntegral . handleError $
                 mapFn dptr (fromIntegral len) sptr
                              (fromIntegral slen) locale
            if n > len
              then go n
              else fromPtr dptr n
      go slen

toLower :: LocaleName -> Text -> Text
toLower = caseMap u_strToLower

toUpper :: LocaleName -> Text -> Text
toUpper = caseMap u_strToUpper

foreign import ccall unsafe "hs_text_icu.h __hs_u_strFoldCase" u_strFoldCase
    :: Ptr UChar -> Int32 -> Ptr UChar -> Int32 -> Word32 -> Ptr UErrorCode
    -> IO Int32

foreign import ccall unsafe "hs_text_icu.h __hs_u_strToLower" u_strToLower
    :: CaseMapper

foreign import ccall unsafe "hs_text_icu.h __hs_u_strToUpper" u_strToUpper
    :: CaseMapper

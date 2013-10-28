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
    -- $case
      toCaseFold
    , toLower
    , toUpper
    ) where

import Data.Int (Int32)
import Data.Text (Text)
import Data.Text.Foreign (fromPtr, useAsPtr)
import Data.Text.ICU.Error.Internal (UErrorCode, handleOverflowError)
import Data.Text.ICU.Internal (LocaleName, UChar, withLocaleName)
import Data.Word (Word32)
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr, castPtr)
import System.IO.Unsafe (unsafePerformIO)

-- $case
--
-- In some languages, case conversion is a locale- and
-- context-dependent operation. The case conversion functions in this
-- module are locale and context sensitive.

-- | Case-fold the characters in a string.
--
-- Case folding is locale independent and not context sensitive, but
-- there is an option for treating the letter I specially for Turkic
-- languages.  The result may be longer or shorter than the original.
toCaseFold :: Bool -- ^ Whether to include or exclude mappings for
                   -- dotted and dotless I and i that are marked with
                   -- 'I' in @CaseFolding.txt@.
           -> Text -> Text
toCaseFold excludeI s = unsafePerformIO .
  useAsPtr s $ \sptr slen -> do
    let opts = fromIntegral . fromEnum $ excludeI
    handleOverflowError (fromIntegral slen)
        (\dptr dlen -> u_strFoldCase dptr dlen sptr (fromIntegral slen) opts)
        (\dptr dlen -> fromPtr (castPtr dptr) (fromIntegral dlen))

type CaseMapper = Ptr UChar -> Int32 -> Ptr UChar -> Int32 -> CString
                -> Ptr UErrorCode -> IO Int32

caseMap :: CaseMapper -> LocaleName -> Text -> Text
caseMap mapFn loc s = unsafePerformIO .
  withLocaleName loc $ \locale ->
    useAsPtr s $ \sptr slen ->
      handleOverflowError (fromIntegral slen)
      (\dptr dlen -> mapFn dptr dlen sptr (fromIntegral slen) locale)
      (\dptr dlen -> fromPtr (castPtr dptr) (fromIntegral dlen))

-- | Lowercase the characters in a string.
--
-- Casing is locale dependent and context sensitive.  The result may
-- be longer or shorter than the original.
toLower :: LocaleName -> Text -> Text
toLower = caseMap u_strToLower

-- | Uppercase the characters in a string.
--
-- Casing is locale dependent and context sensitive.  The result may
-- be longer or shorter than the original.
toUpper :: LocaleName -> Text -> Text
toUpper = caseMap u_strToUpper

foreign import ccall unsafe "hs_text_icu.h __hs_u_strFoldCase" u_strFoldCase
    :: Ptr UChar -> Int32 -> Ptr UChar -> Int32 -> Word32 -> Ptr UErrorCode
    -> IO Int32

foreign import ccall unsafe "hs_text_icu.h __hs_u_strToLower" u_strToLower
    :: CaseMapper

foreign import ccall unsafe "hs_text_icu.h __hs_u_strToUpper" u_strToUpper
    :: CaseMapper

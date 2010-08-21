{-# LANGUAGE DeriveDataTypeable, EmptyDataDecls, ForeignFunctionInterface #-}
-- |
-- Module      : Data.Text.ICU.CaseMap
-- Copyright   : (c) 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Character set case mapping functions for Unicode, implemented as
-- bindings to the International Components for Unicode (ICU)
-- libraries.

module Data.Text.ICU.CaseMap
    (
      CaseMap
    , CaseOption(..)
    , caseMap
    ) where

#include <unicode/ucasemap.h>
#include <unicode/uchar.h>

import Data.Bits ((.|.))
import Data.List (foldl')
import Data.Text.ICU.Error.Internal (UErrorCode, handleError)
import Data.Text.ICU.Internal (LocaleName, withLocaleName)
import Data.Typeable (Typeable)
import Data.Word (Word32)
import Foreign.C.String (CString)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr)
import Foreign.Ptr (FunPtr, Ptr)
import System.IO.Unsafe (unsafePerformIO)

data UCaseMap

data CaseMap = CaseMap {-# UNPACK #-} !(ForeignPtr UCaseMap)
               deriving (Eq, Typeable)

data CaseOption = FoldCaseExcludeSpecialI
                | TitleCaseNoLowerCase
                | TitleCaseNoBreakAdjustment
                  deriving (Eq, Enum)

fromCaseMapOption :: CaseOption -> Word32
fromCaseMapOption FoldCaseExcludeSpecialI    = #const U_FOLD_CASE_EXCLUDE_SPECIAL_I
fromCaseMapOption TitleCaseNoLowerCase       = #const U_TITLECASE_NO_LOWERCASE
fromCaseMapOption TitleCaseNoBreakAdjustment = #const U_TITLECASE_NO_BREAK_ADJUSTMENT

reduceCaseMapOptions :: [CaseOption] -> Word32
reduceCaseMapOptions = foldl' (.|.) (#const U_FOLD_CASE_DEFAULT) .
                       map fromCaseMapOption

caseMap :: LocaleName -> [CaseOption] -> CaseMap
caseMap name opts = unsafePerformIO $ do
  p <- withLocaleName name $ \nptr -> handleError . ucasemap_open nptr .
       reduceCaseMapOptions $ opts
  CaseMap `fmap` newForeignPtr ucasemap_close p

foreign import ccall unsafe "hs_text_icu.h __hs_ucasemap_open" ucasemap_open
    :: CString -> Word32 -> Ptr UErrorCode -> IO (Ptr UCaseMap)

foreign import ccall unsafe "hs_text_icu.h &__hs_ucasemap_close" ucasemap_close
    :: FunPtr (Ptr UCaseMap -> IO ())

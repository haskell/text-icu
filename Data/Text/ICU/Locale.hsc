{-# LANGUAGE BlockArguments, ImportQualifiedPost, RankNTypes, BangPatterns, ForeignFunctionInterface, RecordWildCards #-}
-- |
-- Module      : Data.Text.ICU.Locale
-- Copyright   : (c) 2021 Torsten Kemps-Benedix
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Locale functions implemented as bindings to
-- the International Components for Unicode (ICU) libraries.

module Data.Text.ICU.Locale
    (availableLocales
    ) where

#include <unicode/uloc.h>

import Control.Monad (forM)
import Data.Int (Int32)
import Data.Text (Text, pack)
import Data.Text.Foreign (useAsPtr, withCStringLen)
import Data.Text.ICU.Enumerator
import Data.Text.ICU.Error.Internal (UErrorCode, handleError)
import Data.Text.ICU.Internal (LocaleName(..), UChar, withLocaleName)
import Data.Time.Calendar qualified as Cal
import Data.Time.Clock qualified as Clock
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt(..))
import Foreign.ForeignPtr (newForeignPtr, withForeignPtr, ForeignPtr)
import Foreign.Ptr (FunPtr, Ptr)
import Prelude hiding (last)
import System.IO.Unsafe (unsafePerformIO)

-- |  Get the available default locales, i.e. locales that return data when passed to ICU 
-- APIs, but not including legacy or alias locales. 
availableLocales :: IO [String]
availableLocales = do
  n <- uloc_countAvailable
  forM [0..n-1] \i -> uloc_getAvailable i >>= peekCString

foreign import ccall unsafe "hs_text_icu.h __hs_uloc_getAvailable" uloc_getAvailable
    :: Int32 -> IO CString
foreign import ccall unsafe "hs_text_icu.h __hs_uloc_countAvailable" uloc_countAvailable
    :: IO Int32

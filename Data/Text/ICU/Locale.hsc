{-# LANGUAGE RankNTypes, BangPatterns, ForeignFunctionInterface, RecordWildCards #-}
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
import Foreign.C.String (CString, peekCString)
import Prelude hiding (last)

-- |  Get the available default locales, i.e. locales that return data when passed to ICU 
-- APIs, but not including legacy or alias locales. 
availableLocales :: IO [String]
availableLocales = do
  n <- uloc_countAvailable
  forM [0..n-1] $ \i -> uloc_getAvailable i >>= peekCString

foreign import ccall unsafe "hs_text_icu.h __hs_uloc_getAvailable" uloc_getAvailable
    :: Int32 -> IO CString
foreign import ccall unsafe "hs_text_icu.h __hs_uloc_countAvailable" uloc_countAvailable
    :: IO Int32

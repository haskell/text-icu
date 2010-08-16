-- |
-- Module      : Data.Text.ICU
-- Copyright   : (c) 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Commonly used functions for Unicode, implemented as bindings to the
-- International Components for Unicode (ICU) libraries.
--
-- This module contains only a handful of commonly used types and
-- functions.  Other modules in this package expose richer interfaces.
module Data.Text.ICU
    (
    -- * Normalization
      NormalizationMode(..)
    , normalize
    , NormalizationCheckResult(..)
    , quickCheck
    , isNormalized
    -- * String comparison
    -- ** Normalization-sensitive string comparison
    , CompareOption(..)
    , compare
    -- ** Locale-sensitive string comparison
    , uca
    , collate
    ) where

import Data.Text.ICU.Collate
import Data.Text.ICU.Normalize
import Prelude hiding (compare)
import System.IO.Unsafe (unsafePerformIO)

-- | A 'Collator' that uses the Unicode Collation Algorithm (UCA).
uca :: Collator
uca = unsafePerformIO (open (Just "root"))
{-# NOINLINE uca #-}

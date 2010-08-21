{-# LANGUAGE NoImplicitPrelude #-}
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
-- This module contains only the most commonly used types and
-- functions.  Other modules in this package expose richer interfaces.
module Data.Text.ICU
    (
    -- * Locale support
      LocaleName(..)
    , toCaseFold
    , toLower
    , toUpper
    -- * Iteration
    , CharIterator
    , fromText
    , fromUtf8
    -- * Normalization
    , NormalizationMode(..)
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
    , collateIter
    , sortKey
    ) where

import Data.Text.ICU.Collate
import Data.Text.ICU.Internal
import Data.Text.ICU.Iterator
import Data.Text.ICU.Normalize
import Data.Text.ICU.Text

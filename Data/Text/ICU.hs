{-# LANGUAGE CPP, NoImplicitPrelude #-}
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
    -- * Data representation
    -- $data
    -- * Locale support
      LocaleName(..)
    , toCaseFold
    , toLower
    , toUpper
    -- * Iteration
    , CharIterator
    , fromString
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
#if defined(__HADDOCK__)
import Data.Text.Foreign
import Data.Text (Text)
#endif

-- $data
--
-- The Haskell 'Text' type is implemented as an array in the Haskell
-- heap.  This means that its location is not pinned; it may be copied
-- during a garbage collection pass.  ICU, on the other hand, works
-- with strings that are allocated in the normal system heap and have
-- a fixed address.
--
-- To accommodate this need, these bindings use the functions from
-- 'Data.Text.Foreign' to copy data between the Haskell heap and the
-- system heap.  The copied strings are still managed automatically,
-- but the need to duplicate data does add some performance and memory
-- overhead.

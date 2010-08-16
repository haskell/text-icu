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
module Data.Text.ICU
    (
    -- * Normalization
      NormalizationMode(..)
    , normalize
    , NormalizationCheckResult(..)
    , quickCheck
    , isNormalized
    , CompareOption(..)
    , compare
    ) where

import Data.Text.ICU.Normalize

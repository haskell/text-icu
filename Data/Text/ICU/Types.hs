-- |
-- Module      : Data.Text.ICU.Types
-- Copyright   : (c) 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Functions for manipulating Unicode text, implemented as bindings to
-- the International Components for Unicode (ICU) libraries.
module Data.Text.ICU.Types
    (
    -- * Widely used types
      LocaleName(..)
    -- * Normalization
    , CompareOption(..)
    , NormalizationCheckResult(..)
    , NormalizationMode(..)
    ) where

import Data.Text.ICU.Internal (LocaleName(..))
import Data.Text.ICU.Normalize (CompareOption(..), NormalizationMode(..),
                                NormalizationCheckResult(..))

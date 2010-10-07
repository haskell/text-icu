-- |
-- Module      : Data.Text.ICU.Types
-- Copyright   : (c) 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Types for use when manipulating Unicode text, using the bindings to
-- the International Components for Unicode (ICU) libraries.
module Data.Text.ICU.Types
    (
    -- * Widely used types
      LocaleName(..)
    , ParseError(errError, errLine, errOffset)
    ) where

import Data.Text.ICU.Error.Internal (ParseError(..))
import Data.Text.ICU.Internal (LocaleName(..))

{-# LANGUAGE EmptyDataDecls #-}
-- |
-- Module      : Data.Text.ICU.Break.Internal
-- Copyright   : (c) 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC

module Data.Text.ICU.Break.Types
    (
      BreakIterator(..)
    , UBreakIterator
    ) where

import Data.IORef (IORef)
import Data.Int (Int32)
import Foreign.ForeignPtr (ForeignPtr)
import Data.Text.ICU.Internal (UTextPtr)

-- A boundary breaker.
data BreakIterator a = BR {
      brText :: IORef UTextPtr
    , brStatus :: Int32 -> a
    , brIter :: ForeignPtr UBreakIterator
    }

data UBreakIterator

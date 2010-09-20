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
import Data.Text (Text)
import Foreign.ForeignPtr (ForeignPtr)

-- A boundary breaker.
data BreakIterator a = BR {
      brText :: IORef Text
    , brStatus :: Int32 -> a
    , brIter :: ForeignPtr UBreakIterator
    }

data UBreakIterator

{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Data.Text.ICU.Iterator
-- Copyright   : (c) 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Iteration functions for Unicode, implemented as bindings to the
-- International Components for Unicode (ICU) libraries.
--
-- Functions using these iterators may be more efficient than their
-- counterparts.
module Data.Text.ICU.Iterator
    (
      CharIterator
    , fromText
    , fromUtf8
    ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.ICU.Internal (CharIterator(..))
import Data.Text.ICU.Collate (collateIter, uca)

-- | Construct a 'CharIterator' from a Unicode string.
fromText :: Text -> CharIterator
fromText = CIText
{-# INLINE fromText #-}

-- | Construct a 'CharIterator' from a Unicode string encoded as a
-- UTF-8 'ByteString'.
fromUtf8 :: ByteString -> CharIterator
fromUtf8 = CIUTF8
{-# INLINE fromUtf8 #-}

instance Eq CharIterator where
    a == b = collateIter uca a b == EQ

instance Ord CharIterator where
    compare = collateIter uca

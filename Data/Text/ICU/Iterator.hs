{-# LANGUAGE ForeignFunctionInterface #-}
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
    -- * Types and constructors
      CharIterator
    , fromText
    , fromUtf8
    -- * Functions
    , compareIter
    ) where

import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Text.ICU.Internal (CharIterator(..), UBool, UCharIterator, asOrdering, withCharIterator)
import Data.Text.ICU.Collate (collateIter, uca)
import Foreign.Ptr (Ptr)
import System.IO.Unsafe (unsafePerformIO)

-- | Compare two 'CharIterator's.
compareIter :: CharIterator -> CharIterator -> Ordering
compareIter a b = unsafePerformIO . fmap asOrdering .
  withCharIterator a $ \ ai ->
    withCharIterator b $ \ bi -> u_strCompareIter ai bi 1

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

foreign import ccall unsafe "hs_text_icu.h __hs_u_strCompareIter" u_strCompareIter
    :: Ptr UCharIterator -> Ptr UCharIterator -> UBool -> IO Int32

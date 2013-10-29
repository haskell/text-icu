{-# LANGUAGE DeriveDataTypeable, ForeignFunctionInterface #-}
-- |
-- Module      : Data.Text.ICU.Collate.Pure
-- Copyright   : (c) 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Pure string collation functions for Unicode, implemented as
-- bindings to the International Components for Unicode (ICU)
-- libraries.
--
-- For the impure collation API (which is richer, but less easy to
-- use), see the "Data.Text.ICU.Collate" module.

module Data.Text.ICU.Collate.Pure
    (
    -- * Unicode collation API
    -- $api
      Collator
    , collator
    , collatorWith
    , collate
    , collateIter
    , sortKey
    , uca
    ) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.ICU.Collate.Internal (Collator(..))
import Data.Text.ICU.Internal (CharIterator, LocaleName(..))
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text.ICU.Collate as IO

-- $api
--

-- | Create an immutable 'Collator' for comparing strings.
--
-- If 'Root' is passed as the locale, UCA collation rules will be
-- used.
collator :: LocaleName -> Collator
collator loc = unsafePerformIO $ C `fmap` IO.open loc

-- | Create an immutable 'Collator' with the given 'Attribute's.
collatorWith :: LocaleName -> [IO.Attribute] -> Collator
collatorWith loc atts = unsafePerformIO $ do
  mc <- IO.open loc
  forM_ atts $ IO.setAttribute mc
  return (C mc)

-- | Compare two strings.
collate :: Collator -> Text -> Text -> Ordering
collate (C c) a b = unsafePerformIO $ IO.collate c a b
{-# INLINE collate #-}

-- | Compare two 'CharIterator's.
--
-- If either iterator was constructed from a 'ByteString', it does not
-- need to be copied or converted beforehand, so this function can be
-- quite cheap.
collateIter :: Collator -> CharIterator -> CharIterator -> Ordering
collateIter (C c) a b = unsafePerformIO $ IO.collateIter c a b
{-# INLINE collateIter #-}

-- | Create a key for sorting the 'Text' using the given 'Collator'.
-- The result of comparing two 'ByteString's that have been
-- transformed with 'sortKey' will be the same as the result of
-- 'collate' on the two untransformed 'Text's.
sortKey :: Collator -> Text -> ByteString
sortKey (C c) = unsafePerformIO . IO.sortKey c
{-# INLINE sortKey #-}

-- | A 'Collator' that uses the Unicode Collation Algorithm (UCA).
uca :: Collator
uca = collator Root
{-# NOINLINE uca #-}

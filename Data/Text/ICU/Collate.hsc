{-# LANGUAGE DeriveDataTypeable, ForeignFunctionInterface #-}
-- |
-- Module      : Data.Text.ICU.Collate
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
-- For the impure collation API (which is more flexible, but less easy
-- to use), see the 'Data.Text.ICU.Collate.IO' module.

module Data.Text.ICU.Collate
    (
    -- * Unicode collation API
    -- $api
      Collator
    , open
    , freeze
    , collate
    , collateIter
    , sortKey
    , uca
    ) where

#include <unicode/ucol.h>

import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Text.ICU.Collate.Internal (MCollator, UCollator, withCollator, wrap)
import Data.Text.ICU.Error.Internal (UErrorCode, handleError)
import Data.Text.ICU.Internal (CharIterator, LocaleName(..))
import Data.Typeable (Typeable)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, nullPtr)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text.ICU.Collate.IO as IO

-- $api
--

-- | String collator type.
newtype Collator = C MCollator
    deriving (Typeable)

-- | Open an immutable 'Collator' for comparing strings.
--
-- If 'Root' is passed as the locale, UCA collation rules will be
-- used.
open :: LocaleName -> IO Collator
open loc = C `fmap` IO.open loc

-- | Make a safe copy of a mutable 'MCollator' for use in pure code.
-- Subsequent changes to the 'MCollator' will not affect the state of
-- the returned 'Collator'.
freeze :: MCollator -> IO Collator
freeze c = do
  p <- withCollator c $ \cptr ->
    with (#const U_COL_SAFECLONE_BUFFERSIZE)
      (handleError . ucol_safeClone cptr nullPtr)
  C `fmap` wrap p

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
uca = unsafePerformIO (open Root)
{-# NOINLINE uca #-}

foreign import ccall unsafe "hs_text_icu.h __hs_ucol_safeClone" ucol_safeClone
        :: Ptr UCollator -> Ptr a -> Ptr Int32 -> Ptr UErrorCode
        -> IO (Ptr UCollator)

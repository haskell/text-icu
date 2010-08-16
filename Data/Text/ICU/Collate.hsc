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
    ) where

#include <unicode/ucol.h>

import Data.Int (Int32)
import Data.Text (Text)
import Data.Text.ICU.Collate.Internal (MCollator, UCollator, withCollator, wrap)
import Data.Text.ICU.Error.Internal (UErrorCode, handleError)
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
-- * If 'Nothing' is passed for the locale, the default locale
--   collation rules will be used.
--
-- * If ('Just' @\"\"@) or 'Just' @\"root\"@ is passed, UCA rules will
--   be used.
open :: Maybe String
     -- ^ The locale containing the required collation rules.
     -> IO Collator
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

foreign import ccall unsafe "hs_text_icu.h __hs_ucol_safeClone" ucol_safeClone
        :: Ptr UCollator -> Ptr a -> Ptr Int32 -> Ptr UErrorCode
        -> IO (Ptr UCollator)

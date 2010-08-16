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
-- String collation functions for Unicode, implemented as bindings to
-- the International Components for Unicode (ICU) libraries.
--
-- For the more powerful, impure collation API, see the
-- 'Data.Text.ICU.Collate.IO' module.

module Data.Text.ICU.Collate
    (
    -- * Unicode collation API
    -- $api
      Collator
    , open
    , freeze
    , collate
    ) where

import Data.Text (Text)
import qualified Data.Text.ICU.Collate.IO as IO
import Data.Typeable (Typeable)
import System.IO.Unsafe (unsafePerformIO)

-- $api
--

-- | String collator type.
newtype Collator = C IO.Collator
    deriving (Typeable)

-- | Open a 'Collator' for comparing strings.
--
-- * If 'Nothing' is passed for the locale, the default locale
--   collation rules will be used.
--
-- * If ('Just' @\"\"@) or 'Just' @\"root\"@ is passed, UCA rules will
--   be used.
open :: Maybe String
     -- ^ The locale containing the required collation rules.
     -> IO Collator
open loc = Collator `fmap` IO.open loc

freeze :: IO.Collator -> IO Collator
freeze c = undefined

-- | Compare two strings.
collate :: Collator -> Text -> Text -> Ordering
collate (C c) a b = unsafePerformIO $ IO.collate c a b
{-# INLINE collate #-}

foreign import ccall unsafe "hs_text_icu.h __hs_ucol_safeClone" ucol_safeClone
        :: Ptr UCollator -> Ptr a -> Ptr Int32 -> UErrorCode
        -> Ptr UCollator

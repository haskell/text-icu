{-# LANGUAGE CPP, DeriveDataTypeable, EmptyDataDecls,
    ForeignFunctionInterface #-}
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

module Data.Text.ICU.Collate
    (
    -- * Unicode collation API
    -- $api
      Collator
    , open
    ) where

import Data.Text.ICU.Error.Internal (UErrorCode, handleError)
import Data.Text.ICU.Internal (withName)
import Data.Typeable (Typeable)
import Foreign.C.String (CString)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Ptr (FunPtr, Ptr)

-- $api
--

data UCollator

-- | String collator type.
data Collator = Collator {-# UNPACK #-} !(ForeignPtr UCollator)
                deriving (Eq, Typeable)

withCollator :: Collator -> (Ptr UCollator -> IO a) -> IO a
{-# INLINE withCollator #-}
withCollator (Collator col) action = withForeignPtr col action

open :: String -> IO Collator
open loc =
  fmap Collator . newForeignPtr ucol_close =<< withName loc (handleError . ucol_open)

foreign import ccall unsafe "hs_text_icu.h __hs_ucol_open" ucol_open
    :: CString -> Ptr UErrorCode -> IO (Ptr UCollator)

foreign import ccall unsafe "hs_text_icu.h &__hs_ucol_close" ucol_close
    :: FunPtr (Ptr UCollator -> IO ())

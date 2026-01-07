{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface #-}
-- |
-- Module      : Data.Text.ICU.Collate.Internal
-- Copyright   : (c) 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Internals of the string collation infrastructure.

module Data.Text.ICU.Collate.Internal
    (
    -- * Unicode collation API
      MCollator(..)
    , Collator(..)
    , UCollator
    , withCollator
    , wrap
    ) where

import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (FunPtr, Ptr)
import Data.Text.ICU.Internal (newICUPtr)

-- $api
--

data UCollator

-- | String collator type.
data MCollator = MCollator {-# UNPACK #-} !(ForeignPtr UCollator)

-- | String collator type.
newtype Collator = C MCollator

withCollator :: MCollator -> (Ptr UCollator -> IO a) -> IO a
withCollator (MCollator col) action = withForeignPtr col action
{-# INLINE withCollator #-}

wrap :: IO (Ptr UCollator) -> IO MCollator
wrap = newICUPtr MCollator ucol_close
{-# INLINE wrap #-}

foreign import ccall unsafe "hs_text_icu.h &__hs_ucol_close" ucol_close
    :: FunPtr (Ptr UCollator -> IO ())

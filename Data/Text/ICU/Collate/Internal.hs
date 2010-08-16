{-# LANGUAGE DeriveDataTypeable, EmptyDataDecls, ForeignFunctionInterface #-}
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
      Collator(..)
    , UCollator
    , UCollationResult
    , withCollator
    , wrap
    ) where

import Data.Typeable (Typeable)
import Foreign.C.Types (CInt)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Ptr (FunPtr, Ptr)

-- $api
--

data UCollator

type UCollationResult = CInt

-- | String collator type.
data Collator = Collator {-# UNPACK #-} !(ForeignPtr UCollator)
                deriving (Typeable)

withCollator :: Collator -> (Ptr UCollator -> IO a) -> IO a
withCollator (Collator col) action = withForeignPtr col action
{-# INLINE withCollator #-}

wrap :: Ptr UCollator -> IO Collator
wrap = fmap Collator . newForeignPtr ucol_close
{-# INLINE wrap #-}

foreign import ccall unsafe "hs_text_icu.h &__hs_ucol_close" ucol_close
    :: FunPtr (Ptr UCollator -> IO ())

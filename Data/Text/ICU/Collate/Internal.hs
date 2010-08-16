{-# LANGUAGE DeriveDataTypeable, EmptyDataDecls #-}
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
    ) where

import Data.Typeable (Typeable)
import Foreign.C.Types (CInt)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr)

-- $api
--

data UCollator

type UCollationResult = CInt

-- | String collator type.
data Collator = Collator {-# UNPACK #-} !(ForeignPtr UCollator)
                deriving (Typeable)

withCollator :: Collator -> (Ptr UCollator -> IO a) -> IO a
{-# INLINE withCollator #-}
withCollator (Collator col) action = withForeignPtr col action

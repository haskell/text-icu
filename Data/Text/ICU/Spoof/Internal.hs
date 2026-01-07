{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface #-}
-- |
-- Module      : Data.Text.ICU.Spoof.Internal
-- Copyright   : (c) 2015 Ben Hamilton
--
-- License     : BSD-style
-- Maintainer  : bgertzfield@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Internals of the spoofability check infrastructure.

module Data.Text.ICU.Spoof.Internal
    (
    -- * Unicode spoof checking API
    -- $api
    -- * Types
      MSpoof(..)
    , Spoof(..)
    , USpoof
    -- * Functions
    , withSpoof
    , wrap
    , wrapWithSerialized
    ) where

import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (FunPtr, Ptr)
import Data.Text.ICU.Internal (newICUPtr)

-- $api
-- Low-level operations on spoof checkers.

-- | Opaque handle to a configurable spoof checker.
data USpoof

-- | Configurable spoof checker wrapping an opaque handle
-- and optionally wrapping a previously serialized instance.
data MSpoof = MSpoof {
    serializedBuf :: Maybe (ForeignPtr Word8)
  , spoofPtr :: {-# UNPACK #-} !(ForeignPtr USpoof)
}

-- | Spoof checker type.
newtype Spoof = S MSpoof

-- | Temporarily unwraps an 'MSpoof' to perform operations on its raw 'USpoof'
-- handle.
withSpoof :: MSpoof -> (Ptr USpoof -> IO a) -> IO a
withSpoof (MSpoof _ spoof) = withForeignPtr spoof
{-# INLINE withSpoof #-}

-- | Wraps a raw 'USpoof' handle in an 'MSpoof', closing the handle when
-- the last reference to the object is dropped.
wrap :: IO (Ptr USpoof) -> IO MSpoof
wrap = newICUPtr (MSpoof Nothing) uspoof_close
{-# INLINE wrap #-}

-- | Wraps a previously serialized spoof checker and raw 'USpoof' handle
-- in an 'MSpoof', closing the handle and releasing the 'ForeignPtr' when
-- the last reference to the object is dropped.
wrapWithSerialized :: ForeignPtr Word8 -> IO (Ptr USpoof) -> IO MSpoof
wrapWithSerialized s = newICUPtr (MSpoof $ Just s) uspoof_close
{-# INLINE wrapWithSerialized #-}

foreign import ccall unsafe "hs_text_icu.h &__hs_uspoof_close" uspoof_close
    :: FunPtr (Ptr USpoof -> IO ())

{-# LANGUAGE DeriveDataTypeable, EmptyDataDecls, ForeignFunctionInterface #-}
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
    -- * Unicode collation API
      MSpoof(..)
    , Spoof(..)
    , USpoof
    , withSpoof
    , wrap
    , wrapWithSerialized
    ) where

import Data.Typeable (Typeable)
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Ptr (FunPtr, Ptr)

-- $api
--

data USpoof

-- | Spoof checker type.
data MSpoof = MSpoof {
    serializedBuf :: Maybe (ForeignPtr Word8)
  , spoofPtr :: {-# UNPACK #-} !(ForeignPtr USpoof)
} deriving (Typeable)

-- | Spoof checker type.
newtype Spoof = C MSpoof
    deriving (Typeable)

withSpoof :: MSpoof -> (Ptr USpoof -> IO a) -> IO a
withSpoof (MSpoof _ spoof) = withForeignPtr spoof
{-# INLINE withSpoof #-}

wrap :: Ptr USpoof -> IO MSpoof
wrap = fmap (MSpoof Nothing) . newForeignPtr uspoof_close
{-# INLINE wrap #-}

wrapWithSerialized :: ForeignPtr Word8 -> Ptr USpoof -> IO MSpoof
wrapWithSerialized s = fmap (MSpoof $ Just s) . newForeignPtr uspoof_close
{-# INLINE wrapWithSerialized #-}

foreign import ccall unsafe "hs_text_icu.h &__hs_uspoof_close" uspoof_close
    :: FunPtr (Ptr USpoof -> IO ())

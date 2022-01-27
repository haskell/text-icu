{-# LANGUAGE DeriveDataTypeable, EmptyDataDecls #-}
-- |
-- Module      : Data.Text.ICU.Bidi.Internal
-- Copyright   : (c) Ondrej Palkovsky 2018
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Internal types for Unicode bidirectional algorithm

module Data.Text.ICU.BiDi.Internal
    (
      BiDi(..)
    , UBiDi
    , withBiDi
    ) where

import Data.Typeable (Typeable)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr)

data UBiDi

-- | BiDi object.  /Note/: this structure is not
-- thread safe. It is /not/ safe to use value of this type
-- simultaneously from multiple threads.
newtype BiDi = BiDi (ForeignPtr UBiDi)
                 deriving (Eq, Typeable)

instance Show BiDi where
    show _ = "BiDi"

withBiDi :: BiDi -> (Ptr UBiDi -> IO a) -> IO a
{-# INLINE withBiDi #-}
withBiDi (BiDi cnv) = withForeignPtr cnv

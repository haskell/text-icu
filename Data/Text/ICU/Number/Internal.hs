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

module Data.Text.ICU.Number.Internal
    (
    -- * Unicode collation API
      MNumberFormat(..)
    , NumberFormat(..)
    , UNumberFormat
    , withNumberFormat
    , wrap
    )
where

import Control.Exception (mask_)
import Data.Typeable (Typeable)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Ptr (FunPtr, Ptr)

-- $api
--

data UNumberFormat

-- | This is the number formatter. It can be created with 'formatter'. Use it to format numbers with the 'format' function.
data MNumberFormat = MNumberFormat {-# UNPACK #-} !(ForeignPtr UNumberFormat)
                 deriving (Typeable)

-- | This is the number formatter. It can be created with 'formatter'. Use it to format numbers with the 'format' function.
newtype NumberFormat = C MNumberFormat
    deriving (Typeable)

withNumberFormat :: MNumberFormat -> (Ptr UNumberFormat -> IO a) -> IO a
withNumberFormat (MNumberFormat col) action = withForeignPtr col action
{-# INLINE withNumberFormat #-}

wrap :: IO (Ptr UNumberFormat) -> IO MNumberFormat
wrap a = mask_ $ fmap MNumberFormat $ newForeignPtr unum_close =<< a
{-# INLINE wrap #-}

foreign import ccall unsafe "hs_text_icu.h &__hs_unum_close" unum_close
    :: FunPtr (Ptr UNumberFormat -> IO ())

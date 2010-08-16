{-# LANGUAGE DeriveDataTypeable, EmptyDataDecls, ForeignFunctionInterface #-}
-- |
-- Module      : Data.Text.ICU.Convert.Internal
-- Copyright   : (c) Bryan O'Sullivan 2009
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Low-level character set types and functions.

module Data.Text.ICU.Convert.Internal
    (
      Converter(..)
    , UConverter
    , getName
    , withConverter
    ) where

import Data.Text.ICU.Error.Internal (UErrorCode, handleError)
import Data.Typeable (Typeable)
import Foreign.C.String (CString, peekCString)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr)
import System.IO.Unsafe (unsafePerformIO)

data UConverter

-- | Character set converter type.  /Note/: this structure is not
-- thread safe. It is /not/ safe to use value of this type
-- simultaneously from multiple threads.
data Converter = Converter {-# UNPACK #-} !(ForeignPtr UConverter)
                 deriving (Eq, Typeable)

instance Show Converter where
    show c = "Converter " ++ show (getName c)

withConverter :: Converter -> (Ptr UConverter -> IO a) -> IO a
{-# INLINE withConverter #-}
withConverter (Converter cnv) action = withForeignPtr cnv action

-- | Gets the internal, canonical name of the converter.
getName :: Converter -> String
getName cnv = unsafePerformIO .
  withConverter cnv $ \ptr ->
    peekCString =<< handleError (ucnv_getName ptr)

foreign import ccall unsafe "hs_text_icu.h __hs_ucnv_getName" ucnv_getName
    :: Ptr UConverter -> Ptr UErrorCode -> IO CString

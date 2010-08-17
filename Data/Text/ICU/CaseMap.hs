{-# LANGUAGE DeriveDataTypeable, EmptyDataDecls, ForeignFunctionInterface #-}
-- |
-- Module      : Data.Text.ICU.CaseMap
-- Copyright   : (c) 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Character set case mapping functions for Unicode, implemented as
-- bindings to the International Components for Unicode (ICU)
-- libraries.

module Data.Text.ICU.CaseMap
    (
      CaseMap
    , caseMap
    ) where

import Data.Text.ICU.Error.Internal (UErrorCode, handleError)
import Data.Text.ICU.Internal (withMName)
import Data.Typeable (Typeable)
import Data.Word (Word32)
import Foreign.C.String (CString)
import Foreign.Ptr (FunPtr, Ptr)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr)
import System.IO.Unsafe (unsafePerformIO)

data UCaseMap

data CaseMap = CaseMap {-# UNPACK #-} !(ForeignPtr UCaseMap)
               deriving (Eq, Typeable)

caseMap :: Maybe String -> CaseMap
caseMap name = unsafePerformIO $ do
  p <- withMName name $ \nptr -> handleError $ ucasemap_open nptr 0
  CaseMap `fmap` newForeignPtr ucasemap_close p

foreign import ccall unsafe "hs_text_icu.h __hs_ucasemap_open" ucasemap_open
    :: CString -> Word32 -> Ptr UErrorCode -> IO (Ptr UCaseMap)

foreign import ccall unsafe "hs_text_icu.h &__hs_ucasemap_close" ucasemap_close
    :: FunPtr (Ptr UCaseMap -> IO ())

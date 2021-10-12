{-# LANGUAGE ImportQualifiedPost, EmptyDataDecls, BangPatterns, ForeignFunctionInterface, RecordWildCards #-}
-- |
-- Module      : Data.Text.ICU.Calendar
-- Copyright   : (c) 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Calendar functions implemented as bindings to
-- the International Components for Unicode (ICU) libraries.

module Data.Text.ICU.Enumerator
    (next, toList, createEnumerator, Enumerator, UEnumerator,
    ) where

#include <unicode/uenum.h>

import Control.DeepSeq (NFData(..))
import Control.Monad (forM)
import Data.IORef (newIORef, writeIORef)
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text.Foreign as T
import Data.Text.Foreign (I16, useAsPtr)
import Data.Text.ICU.Calendar.Types
import Data.Text.ICU.Error.Internal (UErrorCode, handleError)
import Data.Text.ICU.Internal (LocaleName(..), UBool, UChar, asBool, withLocaleName)
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt(..))
import Foreign.ForeignPtr (newForeignPtr, withForeignPtr, ForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import Foreign.Storable (peek)
import Prelude hiding (last)
import System.IO.Unsafe (unsafePerformIO)

data UEnumerator

newtype Enumerator = Enumerator {enumeratorToForeignPtr :: ForeignPtr UEnumerator}

createEnumerator :: Ptr UEnumerator -> IO Enumerator
createEnumerator e = do
  enumPtr <- newForeignPtr uenum_close e
  pure $ Enumerator enumPtr

next :: Enumerator -> IO (Maybe Text)
next enum = withForeignPtr (enumeratorToForeignPtr enum) $ \enumPtr ->
  alloca $ \lenPtr -> do
    textPtr <- handleError $ uenum_unext enumPtr lenPtr
    if textPtr == nullPtr 
      then pure Nothing 
      else do
          n <- peek lenPtr
          t <- T.fromPtr textPtr (fromIntegral n)
          pure $ Just t

toList :: Enumerator -> IO [Text]
toList enum = reverse <$> go []
  where 
    go l = do
      mx <- next enum
      case mx of
        Nothing -> pure l
        Just x -> go (x:l)

foreign import ccall unsafe "hs_text_icu.h &__hs_uenum_close" uenum_close
    :: FunPtr (Ptr UEnumerator -> IO ())
foreign import ccall unsafe "hs_text_icu.h __hs_uenum_unext" uenum_unext
    :: Ptr UEnumerator -> Ptr Int32 -> Ptr UErrorCode
    -> IO (Ptr UChar)

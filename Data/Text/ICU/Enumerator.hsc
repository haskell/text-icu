{-# LANGUAGE ImportQualifiedPost, EmptyDataDecls, BangPatterns, ForeignFunctionInterface, RecordWildCards #-}
-- |
-- Module      : Data.Text.ICU.Calendar
-- Copyright   : (c) 2021 Torsten Kemps-Benedix
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

import Data.Int (Int32)
import Data.Text (Text)
import Data.Text.ICU.Error.Internal (UErrorCode, handleError)
import Data.Text.ICU.Internal (UChar, newICUPtr, fromUCharPtr)
import Foreign.ForeignPtr (withForeignPtr, ForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import Foreign.Storable (peek)
import Prelude hiding (last)

data UEnumerator

newtype Enumerator = Enumerator {enumeratorToForeignPtr :: ForeignPtr UEnumerator}

createEnumerator :: IO (Ptr UEnumerator) -> IO Enumerator
createEnumerator = newICUPtr Enumerator uenum_close

next :: Enumerator -> IO (Maybe Text)
next enum = withForeignPtr (enumeratorToForeignPtr enum) $ \enumPtr ->
  alloca $ \lenPtr -> do
    textPtr <- handleError $ uenum_unext enumPtr lenPtr
    if textPtr == nullPtr
      then pure Nothing
      else do
          n <- peek lenPtr
          t <- fromUCharPtr textPtr (fromIntegral n)
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

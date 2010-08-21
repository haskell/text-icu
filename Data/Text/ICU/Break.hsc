{-# LANGUAGE BangPatterns, EmptyDataDecls, ForeignFunctionInterface #-}
-- |
-- Module      : Data.Text.ICU.Break
-- Copyright   : (c) 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- String breaking functions for Unicode, implemented as bindings to
-- the International Components for Unicode (ICU) libraries.
module Data.Text.ICU.Break
    (
      Break(..)
    , BreakIterator
    , break
    , open
    ) where

#include <unicode/ubrk.h>

import Data.Int (Int32)
import Data.Text (Text)
import Data.Text.Foreign (useAsPtr)
import Data.Text.ICU.Error.Internal (UErrorCode, handleError)
import Data.Text.ICU.Internal (LocaleName, UChar, withLocaleName)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Ptr (FunPtr, Ptr)
import System.IO.Unsafe (unsafeInterleaveIO, unsafePerformIO)
import Prelude hiding (break)

-- | The possible types of text boundaries.
data Break = Character
           | Word
           | Line
           | Sentence
             deriving (Eq, Show, Enum)

fromBreak :: Break -> UBreakIteratorType
fromBreak Character = #const UBRK_CHARACTER
fromBreak Word      = #const UBRK_WORD
fromBreak Line      = #const UBRK_LINE
fromBreak Sentence  = #const UBRK_SENTENCE

newtype BreakIterator = BI (ForeignPtr UBreakIterator)

-- | Create a new 'BreakIterator' for locating text boundaries in the
-- specified locale.
open :: Break -> LocaleName -> Text -> IO BreakIterator
open brk loc t = withLocaleName loc $ \locale ->
  useAsPtr t $ \ptr len -> do
    bi <- handleError $ ubrk_open (fromBreak brk) locale ptr (fromIntegral len)
    BI `fmap` newForeignPtr ubrk_close bi

breakWith :: (Int32 -> IO a) -> Break -> LocaleName -> Text -> [a]
breakWith act brk loc t = unsafePerformIO $ do
  BI bi <- open brk loc t
  let loop (#const UBRK_DONE) = return []
      loop b = do
        !c <- act b
        (c:) `fmap` (withForeignPtr bi ubrk_next >>= loop)
  unsafeInterleaveIO $ withForeignPtr bi ubrk_first >>= loop

break :: Break -> LocaleName -> Text -> [Int]
break = breakWith (return . fromIntegral)

type UBreakIteratorType = CInt
data UBreakIterator

foreign import ccall unsafe "hs_text_icu.h __hs_ubrk_open" ubrk_open
    :: UBreakIteratorType -> CString -> Ptr UChar -> Int32 -> Ptr UErrorCode
    -> IO (Ptr UBreakIterator)

foreign import ccall unsafe "hs_text_icu.h &__hs_ubrk_close" ubrk_close
    :: FunPtr (Ptr UBreakIterator -> IO ())

foreign import ccall unsafe "hs_text_icu.h __hs_ubrk_first" ubrk_first
    :: Ptr UBreakIterator -> IO Int32

foreign import ccall unsafe "hs_text_icu.h __hs_ubrk_next" ubrk_next
    :: Ptr UBreakIterator -> IO Int32

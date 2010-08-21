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
    , open
    , break
    , setText
    , first
    , last
    , next
    , previous
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
import Prelude hiding (last)

-- | The possible types of text boundaries.
data Break = Character
           | Word
           | Line
           | Sentence
             deriving (Eq, Enum, Show)

data WordBreak = Uncategorized
               | Number
               | Letter
               | Kana
               | Ideograph
                 deriving (Eq, Show, Enum)

_getStatus :: Int32 -> WordBreak
_getStatus i
    | i < (#const UBRK_WORD_NONE_LIMIT) = Uncategorized
    | i < (#const UBRK_WORD_NUMBER_LIMIT) = Number
    | i < (#const UBRK_WORD_LETTER_LIMIT) = Letter
    | i < (#const UBRK_WORD_KANA_LIMIT) = Kana
    | i < (#const UBRK_WORD_IDEO_LIMIT) = Ideograph
    | otherwise = error $ "unknown word break status " ++ show i
    
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

-- | Set an existing iterator to point at a new piece of text.
setText :: BreakIterator -> Text -> IO ()
setText (BI bi) t =
  useAsPtr t $ \ptr len ->
    withForeignPtr bi $ \p -> handleError $
                              ubrk_setText p ptr (fromIntegral len)

asIndex :: (Ptr UBreakIterator -> IO Int32) -> BreakIterator -> IO (Maybe Int)
asIndex act (BI bi) = do
  ix <- withForeignPtr bi act
  return $! if ix == (#const UBRK_DONE)
            then Nothing
            else Just $! fromIntegral ix

-- | Determine the index of the first character in the text being
-- scanned.  This is not always the same as index 0 of the text.
first :: BreakIterator -> IO (Maybe Int)
first = asIndex ubrk_first

-- | Determine the index immediately /beyond/ the last character in
-- the text being scanned.  This is not always the same as the last
-- character.
last :: BreakIterator -> IO (Maybe Int)
last = asIndex ubrk_last

-- | Determine the index of the text boundary that follows the current
-- text boundary.
next :: BreakIterator -> IO (Maybe Int)
next = asIndex ubrk_next

-- | Determine the index of the text boundary that precedes the current
-- text boundary.
previous :: BreakIterator -> IO (Maybe Int)
previous = asIndex ubrk_previous

type UBreakIteratorType = CInt
data UBreakIterator

foreign import ccall unsafe "hs_text_icu.h __hs_ubrk_open" ubrk_open
    :: UBreakIteratorType -> CString -> Ptr UChar -> Int32 -> Ptr UErrorCode
    -> IO (Ptr UBreakIterator)

foreign import ccall unsafe "hs_text_icu.h __hs_ubrk_setText" ubrk_setText
    :: Ptr UBreakIterator -> Ptr UChar -> Int32 -> Ptr UErrorCode
    -> IO ()

foreign import ccall unsafe "hs_text_icu.h &__hs_ubrk_close" ubrk_close
    :: FunPtr (Ptr UBreakIterator -> IO ())

foreign import ccall unsafe "hs_text_icu.h __hs_ubrk_first" ubrk_first
    :: Ptr UBreakIterator -> IO Int32

foreign import ccall unsafe "hs_text_icu.h __hs_ubrk_last" ubrk_last
    :: Ptr UBreakIterator -> IO Int32

foreign import ccall unsafe "hs_text_icu.h __hs_ubrk_next" ubrk_next
    :: Ptr UBreakIterator -> IO Int32

foreign import ccall unsafe "hs_text_icu.h __hs_ubrk_previous" ubrk_previous
    :: Ptr UBreakIterator -> IO Int32

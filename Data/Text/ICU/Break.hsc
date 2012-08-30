{-# LANGUAGE BangPatterns, ForeignFunctionInterface, RecordWildCards #-}
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
--
-- The text boundary positions are found according to the rules described in
-- Unicode Standard Annex #29, Text Boundaries, and Unicode Standard Annex
-- #14, Line Breaking Properties.  These are available at
-- <http://www.unicode.org/reports/tr14/> and
-- <http://www.unicode.org/reports/tr29/>.

module Data.Text.ICU.Break
    (
    -- * Types
      BreakIterator
    , Line(..)
    , Word(..)
    -- * Breaking functions
    , breakCharacter
    , breakLine
    , breakSentence
    , breakWord
    , clone
    , setText
    -- * Iteration functions
    -- $indices
    , current
    , first
    , last
    , next
    , previous
    , preceding
    , following
    , isBoundary
    -- * Iterator status
    , getStatus
    , getStatuses
    -- * Locales
    , available
    ) where

#include <unicode/ubrk.h>

import Control.Monad (forM)
import Data.IORef (newIORef, writeIORef)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Text.Foreign (I16, useAsPtr)
import Data.Text.ICU.Break.Types (BreakIterator(..), UBreakIterator)
import Data.Text.ICU.Error.Internal (UErrorCode, handleError)
import Data.Text.ICU.Internal (LocaleName(..), UBool, UChar, asBool, withLocaleName)
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt(..))
import Foreign.ForeignPtr (newForeignPtr, withForeignPtr)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import Prelude hiding (last)
import System.IO.Unsafe (unsafePerformIO)

-- $indices
--
-- /Important note/: All of the indices accepted and returned by
-- functions in this module are offsets into the raw UTF-16 text
-- array, /not/ a count of code points.

-- | Line break status.
data Line = Soft                -- ^ A soft line break is a position at
                                -- which a line break is acceptable, but not
                                -- required.
          | Hard
            deriving (Eq, Show, Enum)

-- | Word break status.
data Word = Uncategorized       -- ^ A \"word\" that does not fit into another
                                -- category.  Includes spaces and most
                                -- punctuation.
          | Number              -- ^ A word that appears to be a number.
          | Letter              -- ^ A word containing letters, excluding
                                -- hiragana, katakana or ideographic
                                -- characters.
          | Kana                -- ^ A word containing kana characters.
          | Ideograph           -- ^ A word containing ideographic characters.
            deriving (Eq, Show, Enum)

-- | Break a string on character boundaries.
--
-- Character boundary analysis identifies the boundaries of \"Extended
-- Grapheme Clusters\", which are groupings of codepoints that should be
-- treated as character-like units for many text operations.  Please see
-- Unicode Standard Annex #29, Unicode Text Segmentation,
-- <http://www.unicode.org/reports/tr29/> for additional information on
-- grapheme clusters and guidelines on their use.
breakCharacter :: LocaleName -> Text -> IO (BreakIterator ())
breakCharacter = open (#const UBRK_CHARACTER) (const ())

-- | Break a string on line boundaries.
--
-- Line boundary analysis determines where a text string can be broken when
-- line wrapping. The mechanism correctly handles punctuation and hyphenated
-- words.
breakLine :: LocaleName -> Text -> IO (BreakIterator Line)
breakLine = open (#const UBRK_LINE) asLine
  where
    asLine i
      | i < (#const UBRK_LINE_SOFT_LIMIT) = Soft
      | i < (#const UBRK_LINE_HARD_LIMIT) = Hard
      | otherwise = error $ "unknown line break status " ++ show i

-- | Break a string on sentence boundaries.
--
-- Sentence boundary analysis allows selection with correct interpretation
-- of periods within numbers and abbreviations, and trailing punctuation
-- marks such as quotation marks and parentheses.
breakSentence :: LocaleName -> Text -> IO (BreakIterator ())
breakSentence = open (#const UBRK_SENTENCE) (const ())

-- | Break a string on word boundaries.
--
-- Word boundary analysis is used by search and replace functions, as well
-- as within text editing applications that allow the user to select words
-- with a double click. Word selection provides correct interpretation of
-- punctuation marks within and following words. Characters that are not
-- part of a word, such as symbols or punctuation marks, have word breaks on
-- both sides.
breakWord :: LocaleName -> Text -> IO (BreakIterator Word)
breakWord = open (#const UBRK_WORD) asWord
  where
    asWord i
      | i < (#const UBRK_WORD_NONE_LIMIT) = Uncategorized
      | i < (#const UBRK_WORD_NUMBER_LIMIT) = Number
      | i < (#const UBRK_WORD_LETTER_LIMIT) = Letter
      | i < (#const UBRK_WORD_KANA_LIMIT) = Kana
      | i < (#const UBRK_WORD_IDEO_LIMIT) = Ideograph
      | otherwise = error $ "unknown word break status " ++ show i

-- | Create a new 'BreakIterator' for locating text boundaries in the
-- specified locale.
open :: UBreakIteratorType -> (Int32 -> a) -> LocaleName -> Text
     -> IO (BreakIterator a)
open brk f loc t = withLocaleName loc $ \locale ->
  useAsPtr t $ \ptr len -> do
    bi <- handleError $ ubrk_open brk locale ptr (fromIntegral len)
    r <- newIORef t
    BR r f `fmap` newForeignPtr ubrk_close bi

-- | Point an existing 'BreakIterator' at a new piece of text.
setText :: BreakIterator a -> Text -> IO ()
setText BR{..} t =
  useAsPtr t $ \ptr len -> do
    withForeignPtr brIter $ \p -> handleError $
                                  ubrk_setText p ptr (fromIntegral len)
    writeIORef brText t

-- | Thread safe cloning operation.  This is substantially faster than
-- creating a new 'BreakIterator' from scratch.
clone :: BreakIterator a -> IO (BreakIterator a)
clone BR{..} = do
  bi <- withForeignPtr brIter $ \p ->
        with 1 $ handleError . ubrk_safeClone p nullPtr
  BR brText brStatus `fmap` newForeignPtr ubrk_close bi

asIndex :: (Ptr UBreakIterator -> IO Int32) -> BreakIterator a -> IO (Maybe I16)
asIndex act BR{..} = do
  i <- withForeignPtr brIter act
  return $! if i == (#const UBRK_DONE)
            then Nothing
            else Just $! fromIntegral i

-- | Reset the breaker to the beginning of the text to be scanned.
first :: BreakIterator a -> IO I16
first BR{..} = fromIntegral `fmap` withForeignPtr brIter ubrk_first

-- | Reset the breaker to the end of the text to be scanned.
last :: BreakIterator a -> IO I16
last BR{..} = fromIntegral `fmap` withForeignPtr brIter ubrk_last

-- | Advance the iterator and break at the text boundary that follows the
-- current text boundary.
next :: BreakIterator a -> IO (Maybe I16)
next = asIndex ubrk_next

-- | Advance the iterator and break at the text boundary that precedes the
-- current text boundary.
previous :: BreakIterator a -> IO (Maybe I16)
previous = asIndex ubrk_previous

-- | Determine the text boundary preceding the specified offset.
preceding :: BreakIterator a -> Int -> IO (Maybe I16)
preceding bi i = asIndex (flip ubrk_preceding (fromIntegral i)) bi

-- | Determine the text boundary following the specified offset.
following :: BreakIterator a -> Int -> IO (Maybe I16)
following bi i = asIndex (flip ubrk_following (fromIntegral i)) bi

-- | Return the character index most recently returned by 'next',
-- 'previous', 'first', or 'last'.
current :: BreakIterator a -> IO (Maybe I16)
current = asIndex ubrk_current

-- | Return the status from the break rule that determined the most recently
-- returned break position.  For rules that do not specify a status, a
-- default value of @()@ is returned.
getStatus :: BreakIterator a -> IO a
getStatus BR{..} = brStatus `fmap` withForeignPtr brIter ubrk_getRuleStatus

-- | Return statuses from all of the break rules that determined the most
-- recently returned break position.
getStatuses :: BreakIterator a -> IO [a]
getStatuses BR{..} =
  withForeignPtr brIter $ \brk -> do
    n <- handleError $ ubrk_getRuleStatusVec brk nullPtr 0
    allocaArray (fromIntegral n) $ \ptr -> do
      _ <- handleError $ ubrk_getRuleStatusVec brk ptr n
      map brStatus `fmap` peekArray (fromIntegral n) ptr

-- | Determine whether the specfied position is a boundary position.
-- As a side effect, leaves the iterator pointing to the first
-- boundary position at or after the given offset.
isBoundary :: BreakIterator a -> Int -> IO Bool
isBoundary BR{..} i = asBool `fmap` withForeignPtr brIter (flip ubrk_isBoundary (fromIntegral i))

-- | Locales for which text breaking information is available.  A
-- 'BreakIterator' in a locale in this list will perform the correct
-- text breaking for the locale.
available :: [LocaleName]
available = unsafePerformIO $ do
  n <- ubrk_countAvailable
  forM [0..n-1] $ \i -> ubrk_getAvailable i >>= fmap Locale . peekCString
{-# NOINLINE available #-}

type UBreakIteratorType = CInt

foreign import ccall unsafe "hs_text_icu.h __hs_ubrk_open" ubrk_open
    :: UBreakIteratorType -> CString -> Ptr UChar -> Int32 -> Ptr UErrorCode
    -> IO (Ptr UBreakIterator)

foreign import ccall unsafe "hs_text_icu.h __hs_ubrk_setText" ubrk_setText
    :: Ptr UBreakIterator -> Ptr UChar -> Int32 -> Ptr UErrorCode
    -> IO ()

foreign import ccall unsafe "hs_text_icu.h __hs_ubrk_safeClone" ubrk_safeClone
    :: Ptr UBreakIterator -> Ptr a -> Ptr Int32 -> Ptr UErrorCode
    -> IO (Ptr UBreakIterator)

foreign import ccall unsafe "hs_text_icu.h &__hs_ubrk_close" ubrk_close
    :: FunPtr (Ptr UBreakIterator -> IO ())

foreign import ccall unsafe "hs_text_icu.h __hs_ubrk_current" ubrk_current
    :: Ptr UBreakIterator -> IO Int32

foreign import ccall unsafe "hs_text_icu.h __hs_ubrk_first" ubrk_first
    :: Ptr UBreakIterator -> IO Int32

foreign import ccall unsafe "hs_text_icu.h __hs_ubrk_last" ubrk_last
    :: Ptr UBreakIterator -> IO Int32

foreign import ccall unsafe "hs_text_icu.h __hs_ubrk_next" ubrk_next
    :: Ptr UBreakIterator -> IO Int32

foreign import ccall unsafe "hs_text_icu.h __hs_ubrk_previous" ubrk_previous
    :: Ptr UBreakIterator -> IO Int32

foreign import ccall unsafe "hs_text_icu.h __hs_ubrk_preceding" ubrk_preceding
    :: Ptr UBreakIterator -> Int32 -> IO Int32

foreign import ccall unsafe "hs_text_icu.h __hs_ubrk_following" ubrk_following
    :: Ptr UBreakIterator -> Int32 -> IO Int32

foreign import ccall unsafe "hs_text_icu.h __hs_ubrk_getRuleStatus" ubrk_getRuleStatus
    :: Ptr UBreakIterator -> IO Int32

foreign import ccall unsafe "hs_text_icu.h __hs_ubrk_getRuleStatusVec" ubrk_getRuleStatusVec
    :: Ptr UBreakIterator -> Ptr Int32 -> Int32 -> Ptr UErrorCode -> IO Int32

foreign import ccall unsafe "hs_text_icu.h __hs_ubrk_isBoundary" ubrk_isBoundary
    :: Ptr UBreakIterator -> Int32 -> IO UBool

foreign import ccall unsafe "hs_text_icu.h __hs_ubrk_countAvailable" ubrk_countAvailable
    :: IO Int32

foreign import ccall unsafe "hs_text_icu.h __hs_ubrk_getAvailable" ubrk_getAvailable
    :: Int32 -> IO CString

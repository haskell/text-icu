{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : Data.Text.ICU.BiDi
-- Copyright   : (c) 2018 Ondrej Palkovsky
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Implementation of the Unicode Bidirectional Algorithm. See the documentation
-- of the libicu library for additional details.
--
-- -- /Note/: this module is not thread safe. /Do not/ call the
-- functions on one BiDi object from more than one thread!

module Data.Text.ICU.BiDi
  (
    BiDi
  -- ** Basic functions
  , open
  , openSized
  -- ** Set data
  , setPara
  , setLine
  -- ** Access the BiDi object
  , countParagraphs
  , getParagraphByIndex
  , getProcessedLength
  -- ** Output text
  , writeReordered
  , WriteOption(..)
  -- ** High-level functions
  , reorderParagraphs
  ) where

#include <unicode/ubidi.h>

import Data.Text.ICU.BiDi.Internal
import Foreign.Marshal.Utils (with)
import Foreign.Storable (peek)
import Foreign.Ptr (FunPtr, Ptr)
import Data.Int (Int32, Int16)
import Data.Text.ICU.Error.Internal (UErrorCode, handleError, handleOverflowError)
import Data.Text (Text)
import Data.Text.ICU.Internal (UChar, useAsUCharPtr, fromUCharPtr, newICUPtr)
import Foreign.C.Types (CInt(..))
import Data.List (foldl')
import Data.Bits ((.|.))
import System.IO.Unsafe (unsafePerformIO)
import Data.Traversable (for)

-- | Allocate a BiDi structure.
open :: IO BiDi
open = newICUPtr BiDi ubidi_close ubidi_open

-- | Allocate a BiDi structure with preallocated memory for internal structures.
openSized ::
     Int32 -- ^ is the maximum text or line length that internal memory will be preallocated for.
           -- An attempt to associate this object with a longer text will fail, unless this value is 0.
  -> Int32 -- ^ is the maximum anticipated number of same-level runs that internal memory will be preallocated for.
           -- An attempt to access visual runs on an object that was not preallocated for as many runs as the text was actually resolved to will fail, unless this value is 0.
  -> IO BiDi
openSized maxlen maxruncount =
  newICUPtr BiDi ubidi_close $ handleError (ubidi_openSized maxlen maxruncount)

-- | Perform the Unicode Bidi algorithm. It is defined in the Unicode Standard Annex #9, version 13,
-- also described in The Unicode Standard, Version 4.0.
-- This function takes a piece of plain text containing one or more paragraphs,
-- with or without externally specified embedding levels from styled text and
-- computes the left-right-directionality of each character.
setPara ::
     BiDi
  -> Text
  -> Int32 -- ^ specifies the default level for the text; it is typically 0 (LTR) or 1 (RTL)
  -> IO ()
setPara bidi t paraLevel =
  withBiDi bidi $ \bptr ->
    useAsUCharPtr t $ \sptr slen -> handleError (ubidi_setPara bptr sptr (fromIntegral slen) paraLevel)

-- | Sets a BiDi to contain the reordering information, especially the resolved levels,
-- for all the characters in a line of text
setLine ::
     BiDi -- ^  the parent paragraph object. It must have been set by a successful call to 'setPara'.
  -> Int32 -- ^ is the line's first index into the text
  -> Int32 -- ^ is just behind the line's last index into the text (its last index +1).
  -> BiDi -- ^ is the object that will now represent a line of the text
  -> IO ()
setLine paraBidi start limit lineBidi =
  withBiDi paraBidi $ \paraptr ->
    withBiDi lineBidi $ \lineptr ->
      handleError (ubidi_setLine paraptr start limit lineptr)

-- | Get the number of paragraphs.
countParagraphs :: BiDi -> IO Int32
countParagraphs bidi = withBiDi bidi ubidi_countParagraphs

-- | Get a paragraph, given the index of this paragraph.
getParagraphByIndex ::
     BiDi
  -> Int32 -- ^ is the number of the paragraph, in the range [0..ubidi_countParagraphs(pBiDi)-1].
  -> IO (Int32, Int32) -- ^ index of the first character of the paragraph in the text and limit of the paragraph
getParagraphByIndex bidi paraIndex =
  withBiDi bidi $ \bptr ->
    with 0 $ \pstart ->
      with 0 $ \pend -> do
        handleError (ubidi_getParagraphByIndex bptr paraIndex pstart pend)
        (,) <$> (fromIntegral <$> peek pstart)
            <*> (fromIntegral <$> peek pend)

-- | Get the length of the source text processed by the last call to 'setPara'.
getProcessedLength :: BiDi -> IO Int32
getProcessedLength bidi = withBiDi bidi ubidi_getProcessedLength

data WriteOption =
  DoMirroring
  -- ^ replace characters with the "mirrored" property in RTL runs by their mirror-image mappings
  | InsertLrmForNumeric
  -- ^ surround the run with LRMs if necessary; this is part of the approximate "inverse Bidi" algorithm
  | KeepBaseCombining
  -- ^ keep combining characters after their base characters in RTL runs
  | OutputReverse
  -- ^ write the output in reverse order
  | RemoveBidiControls
  -- ^ remove Bidi control characters (this does not affect InsertLrmForNumeric)
  deriving (Show)

reduceWriteOpts :: [WriteOption] -> Int16
reduceWriteOpts = foldl' orO 0
    where a `orO` b = a .|. fromWriteOption b

fromWriteOption :: WriteOption -> Int16
fromWriteOption DoMirroring   = #const UBIDI_DO_MIRRORING
fromWriteOption InsertLrmForNumeric   = #const UBIDI_INSERT_LRM_FOR_NUMERIC
fromWriteOption KeepBaseCombining   = #const UBIDI_KEEP_BASE_COMBINING
fromWriteOption OutputReverse   = #const UBIDI_OUTPUT_REVERSE
fromWriteOption RemoveBidiControls   = #const UBIDI_REMOVE_BIDI_CONTROLS

-- | Take a BiDi object containing the reordering information for a piece of text
-- (one or more paragraphs) set by 'setPara' or for a line of text set by 'setLine'
-- and write a reordered string to the destination buffer.
writeReordered :: BiDi -> [WriteOption] -> IO Text
writeReordered bidi opts = do
  destLen <- getProcessedLength bidi
  let options' = reduceWriteOpts opts
  withBiDi bidi $ \bptr ->
    handleOverflowError (fromIntegral destLen)
      (\dptr dlen -> ubidi_writeReordered bptr dptr (fromIntegral dlen) options')
      (\dptr dlen -> fromUCharPtr dptr (fromIntegral dlen))

foreign import ccall unsafe "hs_text_icu.h __hs_ubidi_open" ubidi_open
  :: IO (Ptr UBiDi)

foreign import ccall unsafe "hs_text_icu.h __hs_ubidi_openSized" ubidi_openSized
  :: Int32 -> Int32 -> Ptr UErrorCode -> IO (Ptr UBiDi)

foreign import ccall unsafe "hs_text_icu.h &__hs_ubidi_close" ubidi_close
  :: FunPtr (Ptr UBiDi -> IO ())

foreign import ccall unsafe "hs_text_icu.h __hs_ubidi_setPara" ubidi_setPara
  :: Ptr UBiDi -> Ptr UChar -> Int32 -> Int32 -> Ptr UErrorCode -> IO ()

foreign import ccall unsafe "hs_text_icu.h __hs_ubidi_countParagraphs" ubidi_countParagraphs
  :: Ptr UBiDi -> IO Int32

foreign import ccall unsafe "hs_text_icu.h __hs_ubidi_getParagraphByIndex" ubidi_getParagraphByIndex
  :: Ptr UBiDi -> Int32 -> Ptr CInt -> Ptr CInt -> Ptr UErrorCode -> IO ()

foreign import ccall unsafe "hs_text_icu.h __hs_ubidi_getProcessedLength" ubidi_getProcessedLength
  :: Ptr UBiDi -> IO Int32

foreign import ccall unsafe "hs_text_icu.h __hs_ubidi_writeReordered" ubidi_writeReordered
  :: Ptr UBiDi -> Ptr UChar -> Int32 -> Int16 -> Ptr UErrorCode -> IO Int32

foreign import ccall unsafe "hs_text_icu.h __hs_ubidi_setLine" ubidi_setLine
  :: Ptr UBiDi -> Int32 -> Int32 -> Ptr UBiDi -> Ptr UErrorCode -> IO ()

-- | Helper function to reorder a text to a series of paragraphs.
reorderParagraphs :: [WriteOption] -> Text -> [Text]
reorderParagraphs options input =
  unsafePerformIO $ do
    bidi <- open
    setPara bidi input 0
    pcount <- countParagraphs bidi
    lineBidi <- open
    for [0..pcount-1] $ \pidx -> do
        (start,limit) <- getParagraphByIndex bidi pidx
        setLine bidi start limit lineBidi
        writeReordered lineBidi options

{-# LANGUAGE BangPatterns, RecordWildCards #-}
-- |
-- Module      : Data.Text.ICU.Break.Pure
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

module Data.Text.ICU.Break.Pure
    (
    -- * Types
      Breaker
    , Break
    , brkPrefix
    , brkBreak
    , brkSuffix
    , brkStatus
    , Line(..)
    , Word(..)
    -- * Breaking functions
    , breakCharacter
    , breakLine
    , breakSentence
    , breakWord
    -- * Iteration
    , breaks
    , breaksRight
    ) where

import Data.Text (Text, empty)
import Data.Text.Foreign (dropWord16, takeWord16)
import Data.Text.ICU.Break (Line, Word)
import Data.Text.ICU.Break.Types (BreakIterator(..))
import Data.Text.ICU.Internal (LocaleName)
import System.IO.Unsafe (unsafeInterleaveIO, unsafePerformIO)
import qualified Data.Text.ICU.Break as IO

-- | A boundary analyser.
newtype Breaker a = B (BreakIterator a)

new :: (LocaleName -> Text -> IO (BreakIterator a)) -> LocaleName -> Breaker a
new act loc = unsafePerformIO $ B `fmap` act loc empty

-- | Break a string on character boundaries.
--
-- Character boundary analysis identifies the boundaries of "Extended
-- Grapheme Clusters", which are groupings of codepoints that should be
-- treated as character-like units for many text operations.  Please see
-- Unicode Standard Annex #29, Unicode Text Segmentation,
-- <http://www.unicode.org/reports/tr29/> for additional information on
-- grapheme clusters and guidelines on their use.
breakCharacter :: LocaleName -> Breaker ()
breakCharacter = new IO.breakCharacter

-- | Break a string on line boundaries.
--
-- Line boundary analysis determines where a text string can be broken when
-- line wrapping. The mechanism correctly handles punctuation and hyphenated
-- words.
breakLine :: LocaleName -> Breaker Line
breakLine = new IO.breakLine

-- | Break a string on sentence boundaries.
--
-- Sentence boundary analysis allows selection with correct interpretation
-- of periods within numbers and abbreviations, and trailing punctuation
-- marks such as quotation marks and parentheses.
breakSentence :: LocaleName -> Breaker ()
breakSentence = new IO.breakSentence

-- | Break a string on word boundaries.
--
-- Word boundary analysis is used by search and replace functions, as well
-- as within text editing applications that allow the user to select words
-- with a double click. Word selection provides correct interpretation of
-- punctuation marks within and following words. Characters that are not
-- part of a word, such as symbols or punctuation marks, have word breaks on
-- both sides.
breakWord :: LocaleName -> Breaker Word
breakWord = new IO.breakWord

-- | A break in a string.
data Break a = Break {
      brkPrefix :: {-# UNPACK #-} !Text -- ^ Prefix of the current break.
    , brkBreak :: {-# UNPACK #-} !Text  -- ^ Text of the current break.
    , brkSuffix :: {-# UNPACK #-} !Text -- ^ Suffix of the current break.
    , brkStatus :: !a
    -- ^ Status of the current break (only meaningful if 'Line' or 'Word').
    } deriving (Eq, Show)

-- | Return a list of all breaks in a string, from left to right.
breaks :: Breaker a -> Text -> [Break a]
breaks (B b) t = unsafePerformIO $ do
  bi <- IO.clone b
  IO.setText bi t
  let go p = do
        mix <- IO.next bi
        case mix of
          Nothing -> return []
          Just n -> do
            s <- IO.getStatus bi
            let d = n-p
                u = dropWord16 p t
            (Break (takeWord16 p t) (takeWord16 d u) (dropWord16 d u) s :) `fmap` go n
  unsafeInterleaveIO $ go =<< IO.first bi

-- | Return a list of all breaks in a string, from right to left.
breaksRight :: Breaker a -> Text -> [Break a]
breaksRight (B b) t = unsafePerformIO $ do
  bi <- IO.clone b
  IO.setText bi t
  let go p = do
        mix <- IO.previous bi
        case mix of
          Nothing -> return []
          Just n -> do
            s <- IO.getStatus bi
            let d = p-n
                u = dropWord16 n t
            (Break (takeWord16 n t) (takeWord16 d u) (dropWord16 d u) s :) `fmap` go n
  unsafeInterleaveIO $ go =<< IO.last bi

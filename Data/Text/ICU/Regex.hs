{-# LANGUAGE BangPatterns, EmptyDataDecls, MagicHash, RecordWildCards,
    ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : Data.Text.ICU.Regex
-- Copyright   : (c) 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Regular expression support for Unicode, implemented as bindings to
-- the International Components for Unicode (ICU) libraries.
--
-- The syntax and behaviour of ICU regular expressions are Perl-like.
-- For complete details, see the ICU User Guide entry at
-- <http://userguide.icu-project.org/strings/regexp>.
--
-- /Note/: The functions in this module are not thread safe.  For
-- thread safe use, see 'clone' below, or use the pure functions in
-- 'Data.Text.ICU'.

module Data.Text.ICU.Regex
    (
    -- * Types
      MatchOption(..)
    , ParseError(errError, errLine, errOffset)
    , Regex
    -- * Functions
    -- ** Construction
    , regex
    , regex'
    , clone
    -- ** Managing text to search
    , setText
    , getText
    -- ** Inspection
    , pattern
    -- ** Searching
    , find
    , findNext
    -- ** Match groups
    -- $groups
    , groupCount
    , start
    , end
    , start_
    , end_
    ) where

import Data.Text.ICU.Regex.Internal
import qualified Control.Exception as E
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text.Foreign as T
import Data.Text.Foreign (I16)
import Data.Text.ICU.Internal (asBool)
import Data.Text.ICU.Error.Internal (ParseError(..), handleError)
import Data.Word (Word16)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import System.IO.Unsafe (unsafePerformIO)

instance Show Regex where
    show re = "Regex " ++ show (pattern re)

-- $groups
--
-- Capturing groups are numbered starting from zero.  Group zero is
-- always the entire matching text.  Groups greater than zero contain
-- the text matching each capturing group in a regular expression.

-- | Compile a regular expression with the given options.  This is
-- safest to use when the pattern is constructed at run time.
regex' :: [MatchOption] -> Text -> IO (Either ParseError Regex)
regex' opts pat = (Right `fmap` regex opts pat) `E.catch` \(err::ParseError) ->
                  return (Left err)

-- | Set the subject text string upon which the regular expression
-- will look for matches.  This function may be called any number of
-- times, allowing the regular expression pattern to be applied to
-- different strings.
setText :: Regex -> Text -> IO ()
setText Regex{..} t = do
  (hayfp, hayLen) <- T.asForeignPtr t
  withForeignPtr reRe $ \rePtr ->
    withForeignPtr hayfp $ \hayPtr -> handleError $
      uregex_setText rePtr hayPtr (fromIntegral hayLen)
  writeIORef reText $! H hayfp hayLen

-- | Get the subject text that is currently associated with this
-- regular expression object.
getText :: Regex -> IO (ForeignPtr Word16, I16)
getText Regex{..} = do
  H fp len <- readIORef reText
  return (fp, len)

-- | Return the source form of the pattern used to construct this
-- regular expression or match.
pattern :: Regex -> Text
pattern Regex{..} = unsafePerformIO . withForeignPtr reRe $ \rePtr ->
  alloca $ \lenPtr -> do
    textPtr <- handleError $ uregex_pattern rePtr lenPtr
    (T.fromPtr textPtr . fromIntegral) =<< peek lenPtr

-- | Find the first matching substring of the input string that
-- matches the pattern.
--
-- If /n/ is non-negative, the search for a match begins at the
-- specified index, and any match region is reset.
--
-- If /n/ is -1, the search begins at the start of the input region,
-- or at the start of the full string if no region has been specified.
--
-- If a match is found, 'start', 'end', and 'group' will provide more
-- information regarding the match.
find :: Regex -> I16 -> IO Bool
find Regex{..} n =
    fmap asBool . withForeignPtr reRe $ \rePtr -> handleError $
    uregex_find rePtr (fromIntegral n)

-- | Find the next pattern match in the input string.  Begin searching
-- the input at the location following the end of he previous match,
-- or at the start of the string (or region) if there is no previous
-- match.
--
-- If a match is found, 'start', 'end', and 'group' will provide more
-- information regarding the match.
findNext :: Regex -> IO Bool
findNext Regex{..} =
    fmap asBool . withForeignPtr reRe $ handleError . uregex_findNext

-- | Make a copy of a compiled regular expression.  Cloning a regular
-- expression is faster than opening a second instance from the source
-- form of the expression, and requires less memory.
--
-- Note that the current input string and the position of any matched
-- text within it are not cloned; only the pattern itself and and the
-- match mode flags are copied.
--
-- Cloning can be particularly useful to threaded applications that
-- perform multiple match operations in parallel.  Each concurrent RE
-- operation requires its own instance of a 'Regex'.
clone :: Regex -> IO Regex
{-# INLINE clone #-}
clone Regex{..} = do
  fp <- newForeignPtr uregex_close =<< withForeignPtr reRe (handleError . uregex_clone)
  Regex fp `fmap` newIORef (H emptyForeignPtr 0)

-- | Return the number of capturing groups in this regular
-- expression's pattern.
groupCount :: Regex -> IO Int
groupCount Regex{..} =
    fmap fromIntegral . withForeignPtr reRe $ handleError . uregex_groupCount

-- | Returns the index in the input string of the start of the text
-- matched by the specified capture group during the previous match
-- operation.  Returns @-1@ if the capture group was not part of the
-- last match.
start_ :: Regex -> Int -> IO I16
start_ Regex{..} n =
    fmap fromIntegral . withForeignPtr reRe $ \rePtr -> handleError $
    uregex_start rePtr (fromIntegral n)

-- | Returns the index in the input string of the end of the text
-- matched by the specified capture group during the previous match
-- operation.  Returns @-1@ if the capture group was not part of
-- the last match.
end_ :: Regex -> Int -> IO I16
end_ Regex{..} n =
  fmap fromIntegral . withForeignPtr reRe $ \rePtr -> handleError $
  uregex_end rePtr (fromIntegral n)

-- | Returns the index in the input string of the start of the text
-- matched by the specified capture group during the previous match
-- operation.  Returns 'Nothing' if the capture group was not part of
-- the last match.
start :: Regex -> Int -> IO (Maybe I16)
start r n = check `fmap` start_ r n

-- | Returns the index in the input string of the end of the text
-- matched by the specified capture group during the previous match
-- operation.  Returns 'Nothing' if the capture group was not part of
-- the last match.
end :: Regex -> Int -> IO (Maybe I16)
end r n = check `fmap` end_ r n

check :: I16 -> Maybe I16
check (-1) = Nothing
check k    = Just $! fromIntegral k

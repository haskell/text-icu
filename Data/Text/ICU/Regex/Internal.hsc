{-# LANGUAGE BangPatterns, DeriveDataTypeable, EmptyDataDecls,
    ForeignFunctionInterface, MagicHash, RecordWildCards,
    ScopedTypeVariables #-}

-- |
-- Module      : Data.Text.ICU.Regex.Internal
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

module Data.Text.ICU.Regex.Internal
    (
    -- * Types
      MatchOption(..)
    , Haystack(..)
    , Regex(..)
    , URegularExpression
    -- * Functions
    , emptyForeignPtr
    , regex
    , uregex_clone
    , uregex_close
    , uregex_end
    , uregex_find
    , uregex_findNext
    , uregex_getText
    , uregex_group
    , uregex_groupCount
    , uregex_pattern
    , uregex_setText
    , uregex_start
    ) where

import Control.Monad (when)
import Data.IORef (IORef, newIORef)
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Foreign as T
import Data.Text.ICU.Internal (UBool, UChar)
import Data.Text.ICU.Error (isRegexError)
import Data.Text.ICU.Error.Internal (UParseError, UErrorCode,
                                     handleError, handleParseError)
import Data.Typeable (Typeable)
import Data.Word (Word16, Word32)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, touchForeignPtr, withForeignPtr)
import Foreign.Ptr (FunPtr, Ptr)
import System.IO.Unsafe (unsafePerformIO)

#include <unicode/uregex.h>

-- | Options for controlling matching behaviour.
data MatchOption
    = CaseInsensitive
    -- ^ Enable case insensitive matching.
    | Comments
    -- ^ Allow comments and white space within patterns.
    | DotAll
    -- ^ If set, @\'.\'@ matches line terminators. Otherwise @\'.\'@
    -- matching stops at line end.
    | Literal
    -- ^ If set, treat the entire pattern as a literal string.
    --  Metacharacters or escape sequences in the input sequence will
    --  be given no special meaning.
    --
    --  The option 'CaseInsensitive' retains its meanings on matching
    --  when used in conjunction with this option.  Other options
    --  become superfluous.
    | Multiline
    -- ^ Control behaviour of @\'$\'@ and @\'^\'@. If set, recognize
    -- line terminators within string, Otherwise, match only at start
    -- and end of input string.
    | HaskellLines
    -- ^ Haskell-only line endings.  When this mode is enabled, only
    -- @\'\\n\'@ is recognized as a line ending in the behavior of
    -- @\'.\'@, @\'^\'@, and @\'$\'@.
    | UnicodeWord
    -- ^ Unicode word boundaries.  If set, @\'\\\\b\'@ uses the
    -- Unicode TR 29 definition of word boundaries.
    --
    -- /Warning/: Unicode word boundaries are quite different from
    -- traditional regular expression word boundaries.  See
    -- <http://unicode.org/reports/tr29/#Word_Boundaries>.
    | ErrorOnUnknownEscapes
    -- ^ Throw an error on unrecognized backslash escapes.  If set,
    -- fail with an error on patterns that contain backslash-escaped
    -- ASCII letters without a known special meaning.  If this flag is
    -- not set, these escaped letters represent themselves.
    | WorkLimit Int
    -- ^ Set a processing limit for match operations.
    --
    -- Some patterns, when matching certain strings, can run in
    -- exponential time.  For practical purposes, the match operation
    -- may appear to be in an infinite loop.  When a limit is set a
    -- match operation will fail with an error if the limit is
    -- exceeded.
    --
    -- The units of the limit are steps of the match engine.
    -- Correspondence with actual processor time will depend on the
    -- speed of the processor and the details of the specific pattern,
    -- but will typically be on the order of milliseconds.
    --
    -- By default, the matching time is not limited.
    | StackLimit Int
    -- ^ Set the amount of heap storage avaliable for use by the match
    -- backtracking stack.
    --
    -- ICU uses a backtracking regular expression engine, with the
    -- backtrack stack maintained on the heap.  This function sets the
    -- limit to the amount of memory that can be used for this
    -- purpose.  A backtracking stack overflow will result in an error
    -- from the match operation that caused it.
    --
    -- A limit is desirable because a malicious or poorly designed
    -- pattern can use excessive memory, potentially crashing the
    -- process.  A limit is enabled by default.
      deriving (Eq, Show, Typeable)

data Haystack = H (ForeignPtr Word16) {-# UNPACK #-} !T.I16

-- | A compiled regular expression.
--
-- 'Regex' values are usually constructed using the 'regex' or
-- 'regex'' functions.  This type is also an instance of 'IsString',
-- so if you have the @OverloadedStrings@ language extension enabled,
-- you can construct a 'Regex' by simply writing the pattern in
-- quotes (though this does not allow you to specify any 'Option's).
data Regex = Regex {
      reRe :: ForeignPtr URegularExpression
    , reText :: IORef Haystack
    }

emptyForeignPtr :: ForeignPtr Word16
emptyForeignPtr = unsafePerformIO $ fst `fmap` T.asForeignPtr T.empty
{-# NOINLINE emptyForeignPtr #-}

-- | Compile a regular expression with the given options.  This
-- function throws a 'ParseError' if the pattern is invalid.
--
-- The 'Regex' is initialized with empty text to search against.
regex :: [MatchOption] -> Text -> IO Regex
regex opts pat = T.useAsPtr pat $ \pptr plen -> do
  let (flags,workLimit,stackLimit) = toURegexpOpts opts
  ptr <- handleParseError isRegexError $
         uregex_open pptr (fromIntegral plen) flags
  refp <- newForeignPtr uregex_close ptr
  (hayfp, hayLen) <- T.asForeignPtr T.empty
  withForeignPtr refp $ \rePtr ->
    withForeignPtr hayfp $ \hayPtr -> handleError $
      uregex_setText rePtr hayPtr (fromIntegral hayLen)
  when (workLimit > -1) .
    handleError $ uregex_setTimeLimit ptr (fromIntegral workLimit)
  when (stackLimit > -1) .
    handleError $ uregex_setStackLimit ptr (fromIntegral stackLimit)
  touchForeignPtr refp
  Regex refp `fmap` newIORef (H hayfp 0)

data URegularExpression

type URegexpFlag = Word32

toURegexpOpts :: [MatchOption] -> (URegexpFlag,Int,Int)
toURegexpOpts = foldl go (0,-1,-1)
  where
    go (!flag,work,stack) opt = (flag+flag',work',stack')
     where
      flag' = case opt of
                CaseInsensitive       -> #const UREGEX_CASE_INSENSITIVE
                Comments              -> #const UREGEX_COMMENTS
                DotAll                -> #const UREGEX_DOTALL
                Literal               -> #const UREGEX_LITERAL
                Multiline             -> #const UREGEX_MULTILINE
                HaskellLines          -> #const UREGEX_UNIX_LINES
                UnicodeWord           -> #const UREGEX_UWORD
                ErrorOnUnknownEscapes -> #const UREGEX_ERROR_ON_UNKNOWN_ESCAPES
                _                     -> 0
      work' = case opt of
                WorkLimit limit       -> limit
                _                     -> work
      stack' = case opt of
                 StackLimit limit     -> limit
                 _                    -> stack

foreign import ccall unsafe "hs_text_icu.h __hs_uregex_open" uregex_open
    :: Ptr UChar -> Int32 -> Word32 -> Ptr UParseError -> Ptr UErrorCode
    -> IO (Ptr URegularExpression)

foreign import ccall unsafe "hs_text_icu.h &__hs_uregex_close" uregex_close
    :: FunPtr (Ptr URegularExpression -> IO ())

foreign import ccall unsafe "hs_text_icu.h __hs_uregex_clone" uregex_clone
    :: Ptr URegularExpression -> Ptr UErrorCode
    -> IO (Ptr URegularExpression)

foreign import ccall unsafe "hs_text_icu.h __hs_uregex_pattern" uregex_pattern
    :: Ptr URegularExpression -> Ptr Int32 -> Ptr UErrorCode
    -> IO (Ptr UChar)

foreign import ccall unsafe "hs_text_icu.h __hs_uregex_setText" uregex_setText
    :: Ptr URegularExpression -> Ptr UChar -> Int32 -> Ptr UErrorCode -> IO ()

foreign import ccall unsafe "hs_text_icu.h __hs_uregex_getText" uregex_getText
    :: Ptr URegularExpression -> Ptr Int32 -> Ptr UErrorCode -> IO (Ptr UChar)

foreign import ccall unsafe "hs_text_icu.h __hs_uregex_find" uregex_find
    :: Ptr URegularExpression -> Int32 -> Ptr UErrorCode -> IO UBool

foreign import ccall unsafe "hs_text_icu.h __hs_uregex_findNext" uregex_findNext
    :: Ptr URegularExpression -> Ptr UErrorCode -> IO UBool

foreign import ccall unsafe "hs_text_icu.h __hs_uregex_start" uregex_start
    :: Ptr URegularExpression -> Int32 -> Ptr UErrorCode -> IO Int32

foreign import ccall unsafe "hs_text_icu.h __hs_uregex_end" uregex_end
    :: Ptr URegularExpression -> Int32 -> Ptr UErrorCode -> IO Int32

foreign import ccall unsafe "hs_text_icu.h __hs_uregex_groupCount" uregex_groupCount
    :: Ptr URegularExpression -> Ptr UErrorCode -> IO Int32

foreign import ccall unsafe "hs_text_icu.h __hs_uregex_group" uregex_group
    :: Ptr URegularExpression -> Int32 -> Ptr UChar -> Int32 -> Ptr UErrorCode
    -> IO Int32

foreign import ccall unsafe "hs_text_icu.h __hs_uregex_setTimeLimit" uregex_setTimeLimit
    :: Ptr URegularExpression -> Int32 -> Ptr UErrorCode -> IO ()

foreign import ccall unsafe "hs_text_icu.h __hs_uregex_setStackLimit" uregex_setStackLimit
    :: Ptr URegularExpression -> Int32 -> Ptr UErrorCode -> IO ()

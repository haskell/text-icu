{-# LANGUAGE BangPatterns, EmptyDataDecls, ForeignFunctionInterface,
    ScopedTypeVariables #-}

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

module Data.Text.ICU.Regex
    (
    -- * Types
      Option(..)
    , ParseError(errError, errLine, errOffset)
    , Match
    , Regex
    , Regular
    -- * Functions
    -- ** Construction
    , regex
    , regex'
    -- ** Inspection
    , pattern
    -- ** Searching
    , find
    , findAll
    -- ** Match groups
    -- $groups
    , groupCount
    , group
    , prefix
    , suffix
    , context
    ) where

import Control.Exception (catch)
import Control.Monad (when)
import Data.Int (Int32)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Foreign as T
import Data.Text.ICU.Internal (UBool, UChar, asBool)
import Data.Text.ICU.Error (isRegexError)
import Data.Text.ICU.Error.Internal (ParseError(..), UParseError, UErrorCode,
                                     handleError, handleParseError, isFailure,
                                     withError)
import Data.Word (Word16, Word32)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, touchForeignPtr,
                           withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (advancePtr, allocaArray)
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import Foreign.Storable (peek)
import Prelude hiding (catch)
import System.IO.Unsafe (unsafeInterleaveIO, unsafePerformIO)

#include <unicode/uregex.h>

-- | Options for controlling matching behaviour.
data Option
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


-- | A compiled regular expression.
--
-- 'Regex' values are usually constructed using the 'regex' or
-- 'regex'' functions.  This type is also an instance of 'IsString',
-- so if you have the @OverloadedStrings@ language extension enabled,
-- you can construct a 'Regex' by simply writing the pattern in
-- quotes (though this does not allow you to specify any 'Option's).
newtype Regex = Regex {
      reRe :: ForeignPtr URegularExpression
    }

instance Show Regex where
    show re = "Regex " ++ show (pattern re)

instance IsString Regex where
    fromString = regex [] . T.pack

-- | A match for a regular expression.
data Match = Match {
      matchRe :: ForeignPtr URegularExpression
    , _matchFp :: ForeignPtr Word16
    }

instance Show Match where
    show m = "Match" ++ case context 0 m of
                          Nothing -> ""
                          Just x -> ' ' : show x

-- | A typeclass for functions common to both 'Match' and 'Regex'
-- types.
class Regular r where
    regFp :: r -> ForeignPtr URegularExpression

instance Regular Match where
    regFp = matchRe

instance Regular Regex where
    regFp = reRe

regexIO :: [Option] -> Text -> IO Regex
regexIO opts pat = T.useAsPtr pat $ \pptr plen -> do
  let (flags,workLimit,stackLimit) = toURegexpOpts opts
  ptr <- handleParseError isRegexError $
         uregex_open pptr (fromIntegral plen) flags
  fp <- newForeignPtr uregex_close ptr
  when (workLimit /= -1) .
    handleError $ uregex_setTimeLimit ptr (fromIntegral workLimit)
  when (stackLimit /= -1) .
    handleError $ uregex_setStackLimit ptr (fromIntegral stackLimit)
  touchForeignPtr fp
  return (Regex fp)

-- | Compile a regular expression with the given options.  This
-- function throws a 'ParseError' if the pattern is invalid, so it is
-- best for use when the pattern is statically known.
regex :: [Option] -> Text -> Regex
regex opts pat = unsafePerformIO $ regexIO opts pat

-- | Compile a regular expression with the given options.  This is
-- safest to use when the pattern is constructed at run time.
regex' :: [Option] -> Text -> Either ParseError Regex
regex' opts pat = unsafePerformIO $
                  (Right `fmap` regexIO opts pat) `catch` \(err::ParseError) ->
                  return (Left err)

-- | Return the source form of the pattern used to construct this
-- regular expression or match.
pattern :: Regular r => r -> Text
pattern r = unsafePerformIO . withForeignPtr (regFp r) $ \rePtr ->
  alloca $ \lenPtr -> do
    textPtr <- handleError $ uregex_pattern rePtr lenPtr
    (T.fromPtr textPtr . fromIntegral) =<< peek lenPtr

-- | Find the first match for the regular expression in the given text.
find :: Regex -> Text -> Maybe Match
find re0 haystack = unsafePerformIO .
  matching re0 haystack $ \(Regex re) hayfp -> do
    (err,found) <- withForeignPtr re $ withError . uregex_findNext
    if isFailure err || not (asBool found)
      then return Nothing
      else return (Just (Match re hayfp))

-- | Lazily find all matches for the regular expression in the given
-- text.
findAll :: Regex -> Text -> [Match]
findAll re0 haystack = unsafePerformIO . unsafeInterleaveIO $ go 0
  where
    go !n = matching re0 haystack $ \(Regex re) hayfp -> do
      (err,found) <- withForeignPtr re $ \rePtr -> withError $
                     uregex_find rePtr n
      touchForeignPtr hayfp
      if isFailure err || not (asBool found)
        then return []
        else do
          n' <- withForeignPtr re $ \rePtr -> handleError $ uregex_end rePtr 0
          (Match re hayfp:) `fmap` go n'

matching :: Regex -> Text -> (Regex -> ForeignPtr Word16 -> IO a) -> IO a
matching re0 haystack act = do
  (hayfp, hayLen) <- T.asForeignPtr haystack
  r@(Regex re) <- clone re0
  withForeignPtr re $ \rePtr -> do
    withForeignPtr hayfp $ \hayPtr ->
      handleError $ uregex_setText rePtr hayPtr (fromIntegral hayLen)
  act r hayfp

-- $groups
--
-- Capturing groups are numbered starting from zero.  Group zero is
-- always the entire matching text.  Groups greater than zero contain
-- the text matching each capturing group in a regular expression.

-- | Return the number of capturing groups in this regular
-- expression or match's pattern.
groupCount :: Regular r => r -> Int
groupCount r = fromIntegral . unsafePerformIO . withForeignPtr (regFp r) $
               handleError . uregex_groupCount

-- | Return the /n/th capturing group in a match, or 'Nothing' if /n/
-- is out of bounds.
group :: Int -> Match -> Maybe Text
group n m = grouping n m $ \rePtr -> do
  let n' = fromIntegral n
  start <- handleError $ uregex_start rePtr n'
  end <- handleError $ uregex_end rePtr n'
  let len = end - start
  allocaArray (fromIntegral len) $ \dptr -> do
    _ <- handleError $ uregex_group rePtr n' dptr len
    T.fromPtr dptr (fromIntegral len)

-- | Return the prefix of the /n/th capturing group in a match (the
-- text from the start of the string to the start of the match), or
-- 'Nothing' if /n/ is out of bounds.
prefix :: Int -> Match -> Maybe Text
prefix n m = grouping n m $ \rePtr -> do
  start <- handleError $ uregex_start rePtr (fromIntegral n)
  ptr <- handleError $ uregex_getText rePtr nullPtr
  T.fromPtr ptr (fromIntegral start)

-- | Return the suffix of the /n/th capturing group in a match (the
-- text from the end of the match to the end of the string), or
-- 'Nothing' if /n/ is out of bounds.
suffix :: Int -> Match -> Maybe Text
suffix n m = grouping n m $ \rePtr -> alloca $ \lenPtr -> do
  end <- fmap fromIntegral . handleError $ uregex_end rePtr (fromIntegral n)
  ptr <- handleError $ uregex_getText rePtr lenPtr
  len <- fromIntegral `fmap` peek lenPtr
  T.fromPtr (ptr `advancePtr` end) (len - fromIntegral end)

-- | Return ('prefix','group','suffix') of the /n/th capturing group
-- in a match, or 'Nothing' if /n/ is out of bounds.
context :: Int -> Match -> Maybe (Text,Text,Text)
context n m = grouping n m $ \rePtr -> alloca $ \lenPtr -> do
  let n' = fromIntegral n
  start <- fmap fromIntegral . handleError $ uregex_start rePtr n'
  end <- fmap fromIntegral . handleError $ uregex_end rePtr n'
  ptr <- handleError $ uregex_getText rePtr lenPtr
  len <- fromIntegral `fmap` peek lenPtr
  pre <- T.fromPtr ptr start
  grp <- T.fromPtr (ptr `advancePtr` fromIntegral start) (end - start)
  suf <- T.fromPtr (ptr `advancePtr` fromIntegral end) (len - end)
  return (pre,grp,suf)

grouping :: Int -> Match -> (Ptr URegularExpression -> IO a) -> Maybe a
grouping n (Match m fp) act = unsafePerformIO . withForeignPtr m $ \rePtr -> do
  count <- handleError $ uregex_groupCount rePtr
  let n' = fromIntegral n
  if n < 0 || (n' >= count && count > 0)
    then return Nothing
    else do
      ret <- act rePtr
      touchForeignPtr fp
      return (Just ret)

clone :: Regex -> IO Regex
{-# INLINE clone #-}
clone (Regex re) = (fmap Regex . newForeignPtr uregex_close) =<<
                   withForeignPtr re (handleError . uregex_clone)

data URegularExpression

type URegexpFlag = Word32

toURegexpOpts :: [Option] -> (URegexpFlag,Int,Int)
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

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

module Data.Text.ICU.Regex
    (
      Option(..)
    , ParseError(errLine, errOffset)
    , Match
    , Regex
    , regex
    , regex'
    , findAll
    , match
    , matches
    , group
    , groupCount
    ) where

import Control.Exception (catch, throw)
import Control.Monad (when)
import Data.Int (Int32)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Foreign as T
import Data.Text.ICU.Internal (UBool, UChar, asBool)
import Data.Text.ICU.Error (u_BUFFER_OVERFLOW_ERROR, isRegexError)
import Data.Text.ICU.Error.Internal (ParseError(..), UParseError, UErrorCode,
                                     handleError, handleParseError, isFailure,
                                     withError)
import Data.Word (Word16, Word32)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, touchForeignPtr,
                           withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import Foreign.Storable (peek)
import Prelude hiding (catch)
import System.IO.Unsafe (unsafeInterleaveIO, unsafePerformIO)

#include <unicode/uregex.h>

data Option
    = CaseInsensitive
    -- ^ Enable case insensitive matching.
    | Comments
    -- ^ Allow comments and white space within patterns.
    | DotAll
    -- ^ If set, '\'.\'' matches line terminators. Otherwise '.'
    -- matching stops at line end.
    | Literal
    -- ^ If set, treat the entire pattern as a literal string.
    --  Metacharacters or escape sequences in the input sequence will
    --  be given no special meaning.
    --
    --  The options 'CaseInsensitive' and 'UnicodeCase' retain their
    --  meanings on matching when used in conjunction with this
    --  option.  Other options become superfluous.
    | Multiline
    -- ^ Control behavior of '\'$\'' and '\'^\''. If set, recognize
    -- line terminators within string, Otherwise, match only at start
    -- and end of input string.
    | HaskellLines
    -- ^ Haskell-only line endings.  When this mode is enabled, only
    -- '\'\\n\'' is recognized as a line ending in the behavior of
    -- '\'.\'', '\'^\'', and '\'$\''.
    | UnicodeWord
    -- ^ Unicode word boundaries.  If set, '\'\\\\b\'' uses the
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

newtype Regex = Regex {
      reRe :: ForeignPtr URegularExpression
    }

instance Show Regex where
    show re = "Regex " ++ show (pattern re)

instance IsString Regex where
    fromString = regex [] . T.pack

regexIO :: [Option] -> Text -> IO Regex
regexIO opts pat = T.useAsPtr pat $ \pptr plen -> do
  (fmap Regex . newForeignPtr uregex_close) =<<
    handleParseError isRegexError (uregex_open pptr (fromIntegral plen)
                                   (toURegexpFlag opts))

-- | Compile a regular expression with the given options.  Throws a
-- 'ParseError' if the pattern is invalid.
regex :: [Option] -> Text -> Regex
regex opts pat = unsafePerformIO $ regexIO opts pat

-- | Compile a regular expression with the given options.
regex' :: [Option] -> Text -> Either ParseError Regex
regex' opts pat = unsafePerformIO $
                  (Right `fmap` regexIO opts pat) `catch` \(err::ParseError) ->
                  return (Left err)

-- | Return the source form of the pattern used to construct this
-- regular expression.
pattern :: Regex -> Text
pattern re = unsafePerformIO .
  withForeignPtr (reRe re) $ \rePtr ->
    alloca $ \lenPtr -> do
      textPtr <- handleError $ uregex_pattern rePtr lenPtr
      (T.fromPtr textPtr . fromIntegral) =<< peek lenPtr

-- | Find all matches for the regular expression in the given text.
--
-- Each result is a triple consisting of prefix (text from beginning
-- of string to beginning of match), matched text, and suffix (text
-- from end of match to end of string).
--
-- Matches are computed lazily.  For instance, if you only need one
-- match, use only the first result in the list.
findAll :: Regex -> Text -> [(Text,Text,Text)]
findAll re0 haystack = unsafePerformIO $ do
  (hayfp, hayLen) <- T.asForeignPtr haystack
  Regex re <- clone re0
  withForeignPtr hayfp $ \hayPtr ->
    withForeignPtr re $ \rePtr -> handleError $
      uregex_setText rePtr hayPtr (fromIntegral hayLen)
  let go = do
        (err,found) <- withForeignPtr re (withError . uregex_findNext)
        if isFailure err || not (asBool found)
          then return []
          else do
            mse <- withForeignPtr re $ \rePtr -> do
              touchForeignPtr hayfp
              (err0,start) <- withError $ uregex_start rePtr 0
              (err1,end) <- withError $ uregex_end rePtr 0
              return $ if isFailure err0 || isFailure err1
                       then Nothing
                       else Just (start,end)
            case mse of
              Nothing -> return []
              Just (start,end) -> do
                let len = fromIntegral (end - start)
                    pre = T.takeWord16 (fromIntegral start) haystack
                    s = T.dropWord16 (fromIntegral start) haystack
                    mat = T.takeWord16 len s
                    suf = T.dropWord16 len s
                ((pre,mat,suf):) `fmap` go
  unsafeInterleaveIO go

data Match = Match {
      _matchRe :: ForeignPtr URegularExpression
    , _matchFp :: ForeignPtr Word16
    }

instance Show Match where
    show m = "Match " ++ show (groups m)

match :: Regex -> Text -> Maybe Match
match re0 haystack = unsafePerformIO $ do
  (hayfp, hayLen) <- T.asForeignPtr haystack
  Regex re <- clone re0
  (err,found) <- withForeignPtr re $ \rePtr -> do
    withForeignPtr hayfp $ \hayPtr ->
      handleError $ uregex_setText rePtr hayPtr (fromIntegral hayLen)
    withError $ uregex_findNext rePtr
  if isFailure err || not (asBool found)
    then return Nothing
    else return (Just (Match re hayfp))

matches :: Regex -> Text -> [Match]
matches re0 haystack = unsafePerformIO $ do
  (hayfp, hayLen) <- T.asForeignPtr haystack
  r@(Regex re) <- clone re0
  withForeignPtr re $ \rePtr -> do
    withForeignPtr hayfp $ \hayPtr ->
      handleError $ uregex_setText rePtr hayPtr (fromIntegral hayLen)
  let go = do
        (err,found) <- withForeignPtr re (withError . uregex_findNext)
        if isFailure err || not (asBool found)
          then return []
          else do
            Regex re1 <- clone r
            (Match re1 hayfp:) `fmap` go
  unsafeInterleaveIO go

groupCount :: Regex -> Int
groupCount (Regex re) = fromIntegral . unsafePerformIO . withForeignPtr re $
                        handleError . uregex_groupCount

group :: Int -> Match -> Maybe Text
group n (Match m fp) = unsafePerformIO . withForeignPtr m $ \rePtr -> do
  touchForeignPtr fp
  count <- handleError $ uregex_groupCount rePtr
  let n' = fromIntegral n
  if n < 0 || n' >= count
    then return Nothing
    else do
      (err,len) <- withError $ uregex_group rePtr n' nullPtr 0
      when (isFailure err && err /= u_BUFFER_OVERFLOW_ERROR) $
        throw err
      allocaArray (fromIntegral len) $ \dptr -> do
        _ <- handleError $ uregex_group rePtr n' dptr len
        Just `fmap` T.fromPtr dptr (fromIntegral len)

groups :: Match -> [Text]
groups m = go 0
  where go !n = case group n m of
                  Nothing -> []
                  Just x -> x : go (n+1)

clone :: Regex -> IO Regex
{-# INLINE clone #-}
clone (Regex re) = (fmap Regex . newForeignPtr uregex_close) =<<
                   withForeignPtr re (handleError . uregex_clone)

data URegularExpression

type URegexpFlag = Word32

toURegexpFlag :: [Option] -> URegexpFlag
toURegexpFlag = sum . map go
  where go CaseInsensitive       = #const UREGEX_CASE_INSENSITIVE
        go Comments              = #const UREGEX_COMMENTS
        go DotAll                = #const UREGEX_DOTALL
        go Literal               = #const UREGEX_LITERAL
        go Multiline             = #const UREGEX_MULTILINE
        go HaskellLines          = #const UREGEX_UNIX_LINES
        go UnicodeWord           = #const UREGEX_UWORD
        go ErrorOnUnknownEscapes = #const UREGEX_ERROR_ON_UNKNOWN_ESCAPES

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

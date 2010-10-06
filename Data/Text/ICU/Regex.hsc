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
    ) where

import Foreign.C.Types (CInt)

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

type URegexpFlag = CInt

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

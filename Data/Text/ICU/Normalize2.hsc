{-# LANGUAGE EmptyDataDecls, BlockArguments, ImportQualifiedPost, CPP, DeriveDataTypeable, ForeignFunctionInterface #-}
-- |
-- Module      : Data.Text.ICU.Normalize
-- Copyright   : (c) 2009, 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Character set normalization functions for Unicode, implemented as
-- bindings to the International Components for Unicode (ICU)
-- libraries. See http://www.unicode.org/reports/tr15/ for a description
-- of Unicode normalization modes and why these are needed.

module Data.Text.ICU.Normalize2
    (
    -- * Unicode normalization API
    -- $api
    -- * Create normalizers
    NormalizationMode(..), normalizer, nfcNormalizer, nfdNormalizer, nfkcNormalizer, nfkdNormalizer, nfkcCasefoldNormalizer,
    -- * Normalize unicode strings
    nfc, nfd, nfkc, nfkd, nfkcCasefold, normalize, normalizeWith,
    -- * Checks for normalization
    quickCheck, isNormalized,
    -- * Comparison of unicode strings
    compareUnicode, compareUnicode', CompareOption(..), 
    ) where

#ifdef mingw32_HOST_OS
#define U_HAVE_INTTYPES_H 1
#endif

#include <unicode/uchar.h>
#include <unicode/unorm.h>

import Data.Text (Text)
import Data.Text.Foreign (fromPtr, useAsPtr)
import Data.Text.ICU.Error.Internal (UErrorCode, handleError, handleOverflowError)
import Data.Text.ICU.Internal (UBool, UChar, asBool, asOrdering)
import Data.Text.ICU.Normalize.Internal (UNormalizationCheckResult, toNCR)
import Data.Typeable (Typeable)
import Data.Int (Int32)
import Data.Word (Word32)
import Foreign.C.Types (CInt(..))
import Foreign.ForeignPtr (newForeignPtr_, withForeignPtr, ForeignPtr)
import Foreign.Ptr (Ptr, castPtr)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (compare)
import Data.List (foldl')
import Data.Bits ((.|.))

-- $api
--
-- The 'normalize' function transforms Unicode text into an equivalent
-- composed or decomposed form, allowing for easier sorting and
-- searching of text.  'normalize' supports the standard normalization
-- forms described in <http://www.unicode.org/unicode/reports/tr15/>,
-- Unicode Standard Annex #15: Unicode Normalization Forms.
--
-- Characters with accents or other adornments can be encoded in
-- several different ways in Unicode.  For example, take the character A-acute.
-- In Unicode, this can be encoded as a single character (the
-- \"composed\" form):
--
-- @
--      00C1    LATIN CAPITAL LETTER A WITH ACUTE
-- @
--
-- or as two separate characters (the \"decomposed\" form):
--
-- @
--      0041    LATIN CAPITAL LETTER A
--      0301    COMBINING ACUTE ACCENT
-- @
--
-- To a user of your program, however, both of these sequences should
-- be treated as the same \"user-level\" character \"A with acute
-- accent\".  When you are searching or comparing text, you must
-- ensure that these two sequences are treated equivalently.  In
-- addition, you must handle characters with more than one accent.
-- Sometimes the order of a character's combining accents is
-- significant, while in other cases accent sequences in different
-- orders are really equivalent.
--
-- Similarly, the string \"ffi\" can be encoded as three separate letters:
--
-- @
--      0066    LATIN SMALL LETTER F
--      0066    LATIN SMALL LETTER F
--      0069    LATIN SMALL LETTER I
-- @
--
-- or as the single character
--
-- @
--      FB03    LATIN SMALL LIGATURE FFI
-- @
--
-- The \"ffi\" ligature is not a distinct semantic character, and
-- strictly speaking it shouldn't be in Unicode at all, but it was
-- included for compatibility with existing character sets that
-- already provided it.  The Unicode standard identifies such
-- characters by giving them \"compatibility\" decompositions into the
-- corresponding semantic characters.  When sorting and searching, you
-- will often want to use these mappings.
--
-- 'normalize' helps solve these problems by transforming text into
-- the canonical composed and decomposed forms as shown in the first
-- example above.  In addition, you can have it perform compatibility
-- decompositions so that you can treat compatibility characters the
-- same as their equivalents.  Finally, 'normalize' rearranges accents
-- into the proper canonical order, so that you do not have to worry
-- about accent rearrangement on your own.
--
-- Form 'FCD', \"Fast C or D\", is also designed for collation.  It
-- allows to work on strings that are not necessarily normalized with
-- an algorithm (like in collation) that works under \"canonical
-- closure\", i.e., it treats precomposed characters and their
-- decomposed equivalents the same.
--
-- It is not a normalization form because it does not provide for
-- uniqueness of representation. Multiple strings may be canonically
-- equivalent (their NFDs are identical) and may all conform to 'FCD'
-- without being identical themselves.
--
-- The form is defined such that the \"raw decomposition\", the
-- recursive canonical decomposition of each character, results in a
-- string that is canonically ordered. This means that precomposed
-- characters are allowed for as long as their decompositions do not
-- need canonical reordering.
--
-- Its advantage for a process like collation is that all 'NFD' and
-- most 'NFC' texts - and many unnormalized texts - already conform to
-- 'FCD' and do not need to be normalized ('NFD') for such a
-- process. The 'FCD' 'quickCheck' will return 'Yes' for most strings
-- in practice.
--
-- @'normalize' 'FCD'@ may be implemented with 'NFD'.
--
-- For more details on 'FCD' see the collation design document:
-- <http://source.icu-project.org/repos/icu/icuhtml/trunk/design/collation/ICU_collation_design.htm>
--
-- ICU collation performs either 'NFD' or 'FCD' normalization
-- automatically if normalization is turned on for the collator
-- object.  Beyond collation and string search, normalized strings may
-- be useful for string equivalence comparisons,
-- transliteration/transcription, unique representations, etc.
--
-- The W3C generally recommends to exchange texts in 'NFC'.  Note also
-- that most legacy character encodings use only precomposed forms and
-- often do not encode any combining marks by themselves. For
-- conversion to such character encodings the Unicode text needs to be
-- normalized to 'NFC'.  For more usage examples, see the Unicode
-- Standard Annex.

-- | This is an abstract data type holding a reference to the ICU `UNormalizer2` object.
data Normalizer = Normalizer (ForeignPtr UNormalizer2)

data UNormalizer2

-- | Normalization modes analog (but not identical) to the ones in the 
-- 'Data.Text.ICU.Normalize' module.
data NormalizationMode
    = NFD            -- ^ Canonical decomposition.
    | NFKD           -- ^ Compatibility decomposition.
    | NFC            -- ^ Canonical decomposition followed by canonical composition.
    | NFKC           -- ^ Compatibility decomposition followed by canonical composition.
    | NFKCCasefold   -- ^ NFKC with Casefold.
      deriving (Eq, Show, Enum, Typeable)

createNormalizerWith :: (Ptr UErrorCode -> IO (Ptr UNormalizer2)) -> IO Normalizer
createNormalizerWith f = do
    n <- handleError $ f
    -- from the ICU documentation: "Returns an unmodifiable singleton instance of `unorm2_getInstance()`. Do not delete it."
    -- Thats why we use newForeignPtr_ here.
    nPtr <- newForeignPtr_ n
    pure $ Normalizer nPtr

-- | Create a normalizer for a given normalization mode. This function is more similar to 
-- the interface in the 'Data.Text.ICU.Normalize' module.
normalizer :: NormalizationMode -> IO Normalizer
normalizer NFD = nfdNormalizer
normalizer NFKD = nfkdNormalizer
normalizer NFC = nfcNormalizer
normalizer NFKC = nfkcNormalizer
normalizer NFKCCasefold = nfkcCasefoldNormalizer

-- | Create an NFC normalizer.
nfcNormalizer :: IO Normalizer
nfcNormalizer = createNormalizerWith unorm2_getNFCInstance

-- | Create an NFD normalizer.
nfdNormalizer :: IO Normalizer
nfdNormalizer = createNormalizerWith unorm2_getNFDInstance

-- | Create an NFKC normalizer.
nfkcNormalizer :: IO Normalizer
nfkcNormalizer = createNormalizerWith unorm2_getNFKCInstance

-- | Create an NFKD normalizer.
nfkdNormalizer :: IO Normalizer
nfkdNormalizer = createNormalizerWith unorm2_getNFKDInstance

-- | Create an NFKCCasefold normalizer.
nfkcCasefoldNormalizer :: IO Normalizer
nfkcCasefoldNormalizer = createNormalizerWith unorm2_getNFKCCasefoldInstance

-- * Normalization

-- | Normalize a string with the given normalizer.
normalizeWith :: Normalizer -> Text -> Text
normalizeWith (Normalizer nf) t = unsafePerformIO $
  withForeignPtr nf $ \nfPtr -> do
    useAsPtr t $ \sptr slen ->
      let slen' = fromIntegral slen
      in handleOverflowError (fromIntegral slen)
          (\dptr dlen -> unorm2_normalize nfPtr sptr slen' dptr (fromIntegral dlen))
          (\dptr dlen -> fromPtr (castPtr dptr) (fromIntegral dlen))

-- | Normalize a string using the given normalization mode.
normalize :: NormalizationMode -> Text -> Text
normalize NFC = nfc
normalize NFD = nfd
normalize NFKC = nfkc
normalize NFKD = nfkd
normalize NFKCCasefold = nfkcCasefold

-- | Create an NFC normalizer and apply this to the given text.
--
-- Let's have a look at a concrete example that contains the letter a with an acute accent twice. 
-- First as a comination of two codepoints and second as a canonical composite or precomposed 
-- character. Both look exactly the same but one character consists of two and one of only one 
-- codepoint. A bytewise comparison does not give equality of these.
--
-- >>> import Data.Text
-- >>> let t = pack "a\x301á"
-- >>> t
-- "a\769\225"
-- >>> putStr t
-- áá
-- pack "a\x301" == pack "á"
-- False
--
-- But now lets apply some normalization functions and see how these characters coincide afterwards
-- in two different ways:
--
-- >>> nfc t
-- "\225\225"
-- >>> nfd t
-- "a\769a\769"
-- 
-- That is exactly what 'compareUnicode'' does:
--
-- >>> pack "a\x301" `compareUnicode'` pack "á"
nfc :: Text -> Text
nfc t = unsafePerformIO do
  nf <- nfcNormalizer
  pure $ normalizeWith nf t

-- | Create an NFKC normalizer and apply this to the given text.
nfkc :: Text -> Text
nfkc t = unsafePerformIO do
  nf <- nfkcNormalizer
  pure $ normalizeWith nf t

-- | Create an NFD normalizer and apply this to the given text.
nfd :: Text -> Text
nfd t = unsafePerformIO do
  nf <- nfdNormalizer
  pure $ normalizeWith nf t

-- | Create an NFC normalizer and apply this to the given text.
nfkd :: Text -> Text
nfkd t = unsafePerformIO do
  nf <- nfkdNormalizer
  pure $ normalizeWith nf t

-- | Create an NFKCCasefold normalizer and apply this to the given text.
nfkcCasefold :: Text -> Text
nfkcCasefold t = unsafePerformIO do
  nf <- nfkcCasefoldNormalizer
  pure $ normalizeWith nf t

-- * Checks for normalization

-- | Perform an efficient check on a string, to quickly determine if
-- the string is in a particular normalization form.
--
-- A 'Nothing' result indicates that a definite answer could not be
-- determined quickly, and a more thorough check is required,
-- e.g. with 'isNormalized'.  The user may have to convert the string
-- to its normalized form and compare the results.
--
-- A result of 'Just' 'True' or 'Just' 'False' indicates that the
-- string definitely is, or is not, in the given normalization form.
quickCheck :: Normalizer -> Text -> Maybe Bool
quickCheck (Normalizer nf) t = unsafePerformIO $ 
  withForeignPtr nf $ \nfPtr ->
    useAsPtr t $ \sptr slen ->
      fmap toNCR . handleError $ unorm2_quickCheck nfPtr sptr (fromIntegral slen)

-- | Indicate whether a string is in a given normalization form.
--
-- Unlike 'quickCheck', this function returns a definitive result.
-- For 'NFD', 'NFKD', and 'FCD' normalization forms, both functions
-- work in exactly the same ways.  For 'NFC' and 'NFKC' forms, where
-- 'quickCheck' may return 'Nothing', this function will perform
-- further tests to arrive at a definitive result.
isNormalized :: Normalizer -> Text -> Bool
isNormalized (Normalizer nf) t = unsafePerformIO $ 
  withForeignPtr nf $ \nfPtr ->
    useAsPtr t $ \sptr slen ->
      fmap asBool . handleError $ unorm2_isNormalized nfPtr sptr (fromIntegral slen)

-- * Comparison

type UCompareOption = Word32

-- | Options to 'compare'.
data CompareOption = InputIsFCD
                   -- ^ The caller knows that both strings fulfill the
                   -- 'FCD' conditions.  If /not/ set, 'compare' will
                   -- 'quickCheck' for 'FCD' and normalize if
                   -- necessary.
                   | CompareIgnoreCase
                   -- ^ Compare strings case-insensitively using case
                   -- folding, instead of case-sensitively.  If set,
                   -- then the following case folding options are
                   -- used.
                   | FoldCaseExcludeSpecialI
                   -- ^ When case folding, exclude the special I
                   -- character.  For use with Turkic
                   -- (Turkish/Azerbaijani) text data.
                     deriving (Eq, Show, Enum, Typeable)

fromCompareOption :: CompareOption -> UCompareOption
fromCompareOption InputIsFCD              = #const UNORM_INPUT_IS_FCD
fromCompareOption CompareIgnoreCase       = #const U_COMPARE_IGNORE_CASE
fromCompareOption FoldCaseExcludeSpecialI = #const U_FOLD_CASE_EXCLUDE_SPECIAL_I

reduceCompareOptions :: [CompareOption] -> UCompareOption
reduceCompareOptions = foldl' orO (#const U_COMPARE_CODE_POINT_ORDER)
    where a `orO` b = a .|. fromCompareOption b

-- | Compare two strings for canonical equivalence. Further options
-- include case-insensitive comparison and code point order (as
-- opposed to code unit order).
--
-- Canonical equivalence between two strings is defined as their
-- normalized forms ('NFD' or 'NFC') being identical.  This function
-- compares strings incrementally instead of normalizing (and
-- optionally case-folding) both strings entirely, improving
-- performance significantly.
--
-- Bulk normalization is only necessary if the strings do not fulfill
-- the 'FCD' conditions. Only in this case, and only if the strings
-- are relatively long, is memory allocated temporarily.  For 'FCD'
-- strings and short non-'FCD' strings there is no memory allocation.
compareUnicode :: [CompareOption] -> Text -> Text -> Ordering
compareUnicode opts a b = unsafePerformIO do
  useAsPtr a $ \aptr alen ->
    useAsPtr b $ \bptr blen ->
      fmap asOrdering . handleError $
      unorm_compare aptr (fromIntegral alen) bptr (fromIntegral blen)
                    (reduceCompareOptions opts)

-- | This is equivalent to `compareUnicode []`.
compareUnicode' :: Text -> Text -> Ordering
compareUnicode' = compareUnicode []

foreign import ccall unsafe "hs_text_icu.h __hs_unorm2_getNFCInstance" unorm2_getNFCInstance
    :: Ptr UErrorCode 
    -> IO (Ptr UNormalizer2)
foreign import ccall unsafe "hs_text_icu.h __hs_unorm2_getNFDInstance" unorm2_getNFDInstance
    :: Ptr UErrorCode 
    -> IO (Ptr UNormalizer2)
foreign import ccall unsafe "hs_text_icu.h __hs_unorm2_getNFKCInstance" unorm2_getNFKCInstance
    :: Ptr UErrorCode 
    -> IO (Ptr UNormalizer2)
foreign import ccall unsafe "hs_text_icu.h __hs_unorm2_getNFKDInstance" unorm2_getNFKDInstance
    :: Ptr UErrorCode 
    -> IO (Ptr UNormalizer2)
foreign import ccall unsafe "hs_text_icu.h __hs_unorm2_getNFKCCasefoldInstance" unorm2_getNFKCCasefoldInstance
    :: Ptr UErrorCode 
    -> IO (Ptr UNormalizer2)

foreign import ccall unsafe "hs_text_icu.h __hs_unorm_compare" unorm_compare
    :: Ptr UChar -> Int32 
    -> Ptr UChar -> Int32 
    -> Word32
    -> Ptr UErrorCode 
    -> IO Int32

foreign import ccall unsafe "hs_text_icu.h __hs_unorm2_quickCheck" unorm2_quickCheck
    :: Ptr UNormalizer2
    -> Ptr UChar -> Int32 
    -> Ptr UErrorCode
    -> IO UNormalizationCheckResult

foreign import ccall unsafe "hs_text_icu.h __hs_unorm2_isNormalized" unorm2_isNormalized
    :: Ptr UNormalizer2
    -> Ptr UChar -> Int32 
    -> Ptr UErrorCode 
    -> IO UBool

foreign import ccall unsafe "hs_text_icu.h __hs_unorm2_normalize" unorm2_normalize
    :: Ptr UNormalizer2
    -> Ptr UChar -> Int32 
    -> Ptr UChar -> Int32 
    -> Ptr UErrorCode 
    -> IO Int32

{-# LANGUAGE CPP, DeriveDataTypeable, ForeignFunctionInterface #-}
-- |
-- Module      : Data.Text.ICU.Collate
-- Copyright   : (c) 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- String collation functions for Unicode, implemented as bindings to
-- the International Components for Unicode (ICU) libraries.

module Data.Text.ICU.Collate
    (
    -- * Unicode collation API
    -- $api
    -- * Types
      MCollator
    , Attribute(..)
    , AlternateHandling(..)
    , CaseFirst(..)
    , Strength(..)
    -- * Functions
    , open
    , collate
    , collateIter
    -- ** Utility functions
    , equals
    , getAttribute
    , setAttribute
    , sortKey
    , clone
    , freeze
    ) where

#include <unicode/ucol.h>

import Data.ByteString (empty)
import Data.ByteString.Internal (ByteString(..), create, mallocByteString,
                                 memcpy)
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Foreign (useAsPtr)
import Data.Text.ICU.Collate.Internal (Collator(..), MCollator, UCollator,
                                       equals, withCollator, wrap)
import Data.Text.ICU.Error.Internal (UErrorCode, handleError)
import Data.Text.ICU.Internal
    (LocaleName, UChar, CharIterator, UCharIterator,
     asOrdering, withCharIterator, withLocaleName)
import Data.Typeable (Typeable)
import Data.Word (Word8)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, nullPtr)

-- $api
--

-- | Control the handling of variable weight elements.
data AlternateHandling = NonIgnorable
                       -- ^ Treat all codepoints with non-ignorable primary
                       -- weights in the same way.
                       | Shifted
                         -- ^ Cause codepoints with primary weights that are
                         -- equal to or below the variable top value to be
                         -- ignored on primary level and moved to the
                         -- quaternary level.
                         deriving (Eq, Bounded, Enum, Show, Typeable)

-- | Control the ordering of upper and lower case letters.
data CaseFirst = UpperFirst     -- ^ Force upper case letters to sort before
                                -- lower case.
               | LowerFirst     -- ^ Force lower case letters to sort before
                                -- upper case.
                deriving (Eq, Bounded, Enum, Show, Typeable)

-- | The strength attribute. The usual strength for most locales (except
-- Japanese) is tertiary. Quaternary strength is useful when combined with
-- shifted setting for alternate handling attribute and for JIS x 4061
-- collation, when it is used to distinguish between Katakana and Hiragana
-- (this is achieved by setting 'HiraganaQuaternaryMode' mode to
-- 'True'). Otherwise, quaternary level is affected only by the number of
-- non ignorable code points in the string. Identical strength is rarely
-- useful, as it amounts to codepoints of the 'NFD' form of the string.
data Strength = Primary
              | Secondary
              | Tertiary
              | Quaternary
              | Identical
                deriving (Eq, Bounded, Enum, Show, Typeable)

data Attribute = French Bool
               -- ^ Direction of secondary weights, used in French.  'True',
               -- results in secondary weights being considered backwards,
               -- while 'False' treats secondary weights in the order in
               -- which they appear.
               | AlternateHandling AlternateHandling
                 -- ^ For handling variable elements.  'NonIgnorable' is
                 -- default.
               | CaseFirst (Maybe CaseFirst)
               -- ^ Control the ordering of upper and lower case letters.
               -- 'Nothing' (the default) orders upper and lower case
               -- letters in accordance to their tertiary weights.
               | CaseLevel Bool
                 -- ^ Controls whether an extra case level (positioned
                 -- before the third level) is generated or not.  When
                 -- 'False' (default), case level is not generated; when
                 -- 'True', the case level is generated. Contents of the
                 -- case level are affected by the value of the 'CaseFirst'
                 -- attribute. A simple way to ignore accent differences in
                 -- a string is to set the strength to 'Primary' and enable
                 -- case level.
               | NormalizationMode Bool
               -- ^ Controls whether the normalization check and necessary
               -- normalizations are performed. When 'False' (default) no
               -- normalization check is performed. The correctness of the
               -- result is guaranteed only if the input data is in
               -- so-called 'FCD' form (see users manual for more info).
               -- When 'True', an incremental check is performed to see
               -- whether the input data is in 'FCD' form. If the data is
               -- not in 'FCD' form, incremental 'NFD' normalization is
               -- performed.
               | Strength Strength
               | HiraganaQuaternaryMode Bool
                 -- ^ When turned on, this attribute positions Hiragana
                 -- before all non-ignorables on quaternary level. This is a
                 -- sneaky way to produce JIS sort order.
               | Numeric Bool
                 -- ^ When enabled, this attribute generates a collation key
                 -- for the numeric value of substrings of digits.  This is
                 -- a way to get '100' to sort /after/ '2'.
                 deriving (Eq, Show, Typeable)

type UColAttribute = CInt
type UColAttributeValue = CInt

toUAttribute :: Attribute -> (UColAttribute, UColAttributeValue)
toUAttribute (French v)
    = ((#const UCOL_FRENCH_COLLATION), toOO v)
toUAttribute (AlternateHandling v)
    = ((#const UCOL_ALTERNATE_HANDLING), toAH v)
toUAttribute (CaseFirst v)
    = ((#const UCOL_CASE_FIRST), toCF v)
toUAttribute (CaseLevel v)
    = ((#const UCOL_CASE_LEVEL), toOO v)
toUAttribute (NormalizationMode v)
    = ((#const UCOL_NORMALIZATION_MODE), toOO v)
toUAttribute (Strength v)
    = ((#const UCOL_STRENGTH), toS v)
toUAttribute (HiraganaQuaternaryMode v)
    = ((#const UCOL_HIRAGANA_QUATERNARY_MODE), toOO v)
toUAttribute (Numeric v)
    = ((#const UCOL_NUMERIC_COLLATION), toOO v)

toOO :: Bool -> UColAttributeValue
toOO False = #const UCOL_OFF
toOO True  = #const UCOL_ON

toAH :: AlternateHandling -> UColAttributeValue
toAH NonIgnorable = #const UCOL_NON_IGNORABLE
toAH Shifted      = #const UCOL_SHIFTED

toCF :: Maybe CaseFirst -> UColAttributeValue
toCF Nothing           = #const UCOL_OFF
toCF (Just UpperFirst) = #const UCOL_UPPER_FIRST
toCF (Just LowerFirst) = #const UCOL_LOWER_FIRST

toS :: Strength -> UColAttributeValue
toS Primary    = #const UCOL_PRIMARY
toS Secondary  = #const UCOL_SECONDARY
toS Tertiary   = #const UCOL_TERTIARY
toS Quaternary = #const UCOL_QUATERNARY
toS Identical  = #const UCOL_IDENTICAL

fromOO :: UColAttributeValue -> Bool
fromOO (#const UCOL_OFF) = False
fromOO (#const UCOL_ON)  = True
fromOO bad = valueError "fromOO" bad

fromAH :: UColAttributeValue -> AlternateHandling
fromAH (#const UCOL_NON_IGNORABLE) = NonIgnorable
fromAH (#const UCOL_SHIFTED)       = Shifted
fromAH bad = valueError "fromAH" bad

fromCF :: UColAttributeValue -> Maybe CaseFirst
fromCF (#const UCOL_OFF)         = Nothing
fromCF (#const UCOL_UPPER_FIRST) = Just UpperFirst
fromCF (#const UCOL_LOWER_FIRST) = Just LowerFirst
fromCF bad = valueError "fromCF" bad

fromS :: UColAttributeValue -> Strength
fromS (#const UCOL_PRIMARY)    = Primary
fromS (#const UCOL_SECONDARY)  = Secondary
fromS (#const UCOL_TERTIARY)   = Tertiary
fromS (#const UCOL_QUATERNARY) = Quaternary
fromS (#const UCOL_IDENTICAL)  = Identical
fromS bad = valueError "fromS" bad

fromUAttribute :: UColAttribute -> UColAttributeValue -> Attribute
fromUAttribute key val =
  case key of
    (#const UCOL_FRENCH_COLLATION)         -> French (fromOO val)
    (#const UCOL_ALTERNATE_HANDLING)       -> AlternateHandling (fromAH val)
    (#const UCOL_CASE_FIRST)               -> CaseFirst (fromCF val)
    (#const UCOL_CASE_LEVEL)               -> CaseLevel (fromOO val)
    (#const UCOL_NORMALIZATION_MODE)       -> NormalizationMode (fromOO val)
    (#const UCOL_STRENGTH)                 -> Strength (fromS val)
    (#const UCOL_HIRAGANA_QUATERNARY_MODE) -> HiraganaQuaternaryMode (fromOO val)
    (#const UCOL_NUMERIC_COLLATION)        -> Numeric (fromOO val)
    _ -> valueError "fromUAttribute" key

valueError :: Show a => String -> a -> z
valueError func bad = error ("Data.Text.ICU.Collate.IO." ++ func ++
                             ": invalid value " ++ show bad)

type UCollationResult = CInt

-- | Open a 'Collator' for comparing strings.
open :: LocaleName
     -- ^ The locale containing the required collation rules.
     -> IO MCollator
open loc = wrap =<< withLocaleName loc (handleError . ucol_open)

-- | Set the value of an 'MCollator' attribute.
setAttribute :: MCollator -> Attribute -> IO ()
setAttribute c a =
  withCollator c $ \cptr ->
    handleError $ uncurry (ucol_setAttribute cptr) (toUAttribute a)

-- | Get the value of an 'MCollator' attribute.
--
-- It is safe to provide a dummy argument to an 'Attribute' constructor when
-- using this function, so the following will work:
--
-- > getAttribute mcol (NormalizationMode undefined)
getAttribute :: MCollator -> Attribute -> IO Attribute
getAttribute c a = do
  let name = fst (toUAttribute a)
  val <- withCollator c $ \cptr -> handleError $ ucol_getAttribute cptr name
  return $! fromUAttribute name val

-- | Compare two strings.
collate :: MCollator -> Text -> Text -> IO Ordering
collate c a b =
  withCollator c $ \cptr ->
    useAsPtr a $ \aptr alen ->
      useAsPtr b $ \bptr blen ->
        fmap asOrdering . handleError $
        ucol_strcoll cptr aptr (fromIntegral alen) bptr (fromIntegral blen)

-- | Compare two 'CharIterator's.
--
-- If either iterator was constructed from a 'ByteString', it does not need
-- to be copied or converted internally, so this function can be quite
-- cheap.
collateIter :: MCollator -> CharIterator -> CharIterator -> IO Ordering
collateIter c a b =
  fmap asOrdering . withCollator c $ \cptr ->
    withCharIterator a $ \ai ->
      withCharIterator b $ handleError . ucol_strcollIter cptr ai

-- | Create a key for sorting the 'Text' using the given 'Collator'.
-- The result of comparing two 'ByteString's that have been
-- transformed with 'sortKey' will be the same as the result of
-- 'collate' on the two untransformed 'Text's.
sortKey :: MCollator -> Text -> IO ByteString
sortKey c t
    | T.null t = return empty
    | otherwise = do
  withCollator c $ \cptr ->
    useAsPtr t $ \tptr tlen -> do
      let len = fromIntegral tlen
          loop n = do
            fp <- mallocByteString (fromIntegral n)
            i <- withForeignPtr fp $ \p -> ucol_getSortKey cptr tptr len p n
            let j = fromIntegral i
            case undefined of
              _ | i == 0         -> error "Data.Text.ICU.Collate.IO.sortKey: internal error"
                | i > n          -> loop i
                | i <= n `div` 2 -> create j $ \p -> withForeignPtr fp $ \op ->
                                    memcpy p op (fromIntegral i)
                | otherwise      -> return $! PS fp 0 j
      loop (min (len * 4) 8)

-- | Make a safe copy of a mutable 'MCollator' for use in pure code.
-- Subsequent changes to the 'MCollator' will not affect the state of
-- the returned 'Collator'.
freeze :: MCollator -> IO Collator
freeze = fmap C . clone

-- | Make a copy of a mutable 'MCollator'.
-- Subsequent changes to the input 'MCollator' will not affect the state of
-- the returned 'MCollator'.
clone :: MCollator -> IO MCollator
clone c = do
  p <- withCollator c $ \cptr ->
    with (#const U_COL_SAFECLONE_BUFFERSIZE)
      (handleError . ucol_safeClone cptr nullPtr)
  wrap p

foreign import ccall unsafe "hs_text_icu.h __hs_ucol_open" ucol_open
    :: CString -> Ptr UErrorCode -> IO (Ptr UCollator)

foreign import ccall unsafe "hs_text_icu.h __hs_ucol_getAttribute" ucol_getAttribute
    :: Ptr UCollator -> UColAttribute -> Ptr UErrorCode -> IO UColAttributeValue

foreign import ccall unsafe "hs_text_icu.h __hs_ucol_setAttribute" ucol_setAttribute
    :: Ptr UCollator -> UColAttribute -> UColAttributeValue -> Ptr UErrorCode -> IO ()

foreign import ccall unsafe "hs_text_icu.h __hs_ucol_strcoll" ucol_strcoll
    :: Ptr UCollator -> Ptr UChar -> Int32 -> Ptr UChar -> Int32
    -> Ptr UErrorCode -> IO UCollationResult

foreign import ccall unsafe "hs_text_icu.h __hs_ucol_getSortKey" ucol_getSortKey
    :: Ptr UCollator -> Ptr UChar -> Int32 -> Ptr Word8 -> Int32
    -> IO Int32

foreign import ccall unsafe "hs_text_icu.h __hs_ucol_strcollIter" ucol_strcollIter
    :: Ptr UCollator -> Ptr UCharIterator -> Ptr UCharIterator -> Ptr UErrorCode
    -> IO UCollationResult

foreign import ccall unsafe "hs_text_icu.h __hs_ucol_safeClone" ucol_safeClone
        :: Ptr UCollator -> Ptr a -> Ptr Int32 -> Ptr UErrorCode
        -> IO (Ptr UCollator)

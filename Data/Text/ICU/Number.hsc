{-# LANGUAGE CPP, DeriveDataTypeable, ForeignFunctionInterface #-}
-- |
-- Module      : Data.Text.ICU.Number
-- Copyright   : (c) 2020 Torsten Kemps-Benedix
--
-- License     : BSD-style
-- Maintainer  : tkx68@icloud.com
-- Stability   : experimental
-- Portability : GHC
--
-- New users with are strongly encouraged to see
-- if Data.Text.ICU.NumberFormatter fits their use case.
-- Although not deprecated, this header is provided for backwards
-- compatibility only.

module Data.Text.ICU.Number
    (
    -- * Unicode number formatting API
    -- $api
    numberFormatter
    , FormattableNumber, formatNumber, formatNumber'
    , NumberFormatStyle(..)
    , NumberFormat
) where

#ifdef mingw32_HOST_OS
#define U_HAVE_INTTYPES_H 1
#endif

#include <unicode/unum.h>
#include <unicode/parseerr.h>

import GHC.Natural
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.ICU.Error
import Data.Text.ICU.Error.Internal (UErrorCode, UParseError, handleParseError, handleOverflowError)
import Data.Text.ICU.Internal (UChar, useAsUCharPtr, fromUCharPtr)
import Data.Text.ICU.Internal (LocaleName, withLocaleName)
import Data.Typeable (Typeable)
import Data.Int (Int32)
import Foreign.C.Types (CInt(..), CDouble(..))
import Foreign.Ptr (Ptr)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (compare)
import Foreign.C.String (CString)
import Data.Text.ICU.Number.Internal
-- $api
--
-- This module helps you to format and parse numbers for any locale. Your code
-- can be completely independent of the locale conventions for decimal points,
-- thousands-separators, or even the particular decimal digits used, or whether
-- the number format is even decimal. There are different number format styles
-- like decimal, currency, percent and spelled-out.
--
-- Use 'formatter' to create a formatter and 'format' to format numbers.

-- | The possible number format styles.
data NumberFormatStyle
    = NUM_PATTERN_DECIMAL Text -- ^ Decimal format defined by a pattern string. See the section \"Patterns\" at <https://unicode-org.github.io/icu-docs/apidoc/released/icu4c/classDecimalFormat.html#Patterns> for further details regarding pattern strings.
    | NUM_DECIMAL -- ^ Decimal format ("normal" style).
    | NUM_CURRENCY -- ^ Currency format (generic). Defaults to UNUM_CURRENCY_STANDARD style (using currency symbol, e.g., "$1.00", with non-accounting style for negative values e.g. using minus sign). The specific style may be specified using the -cf- locale key.
    | NUM_PERCENT -- ^ Percent format.
    | NUM_SCIENTIFIC -- ^ Scientific format.
    | NUM_SPELLOUT -- ^ Spellout rule-based format. The default ruleset can be specified/changed using unum_setTextAttribute with UNUM_DEFAULT_RULESET; the available public rulesets can be listed using unum_getTextAttribute with UNUM_PUBLIC_RULESETS.
    | NUM_ORDINAL -- ^ Ordinal rule-based format. The default ruleset can be specified/changed using unum_setTextAttribute with UNUM_DEFAULT_RULESET; the available public rulesets can be listed using unum_getTextAttribute with UNUM_PUBLIC_RULESETS.
    | NUM_DURATION -- ^ Duration rule-based format.
    | NUM_NUMBERING_SYSTEM -- ^ Numbering system rule-based format.
    | NUM_PATTERN_RULEBASED Text -- ^ Rule-based format defined by a pattern string.  See the section \"Patterns\" at <https://unicode-org.github.io/icu-docs/apidoc/released/icu4c/classDecimalFormat.html#Patterns> for further details regarding pattern strings.
    | NUM_CURRENCY_ISO -- ^ Currency format with an ISO currency code, e.g., "USD1.00".
    | NUM_CURRENCY_PLURAL -- ^ Currency format with a pluralized currency name, e.g., "1.00 US dollar" and "3.00 US dollars".
    | NUM_CURRENCY_ACCOUNTING -- ^ Currency format for accounting, e.g., "($3.00)" for negative currency amount instead of "-$3.00" (UNUM_CURRENCY). Overrides any style specified using -cf- key in locale.
    | NUM_CASH_CURRENCY -- ^ Currency format with a currency symbol given CASH usage, e.g., "NT$3" instead of "NT$3.23".
    | NUM_DECIMAL_COMPACT_SHORT -- ^ Decimal format expressed using compact notation (short form, corresponds to UNumberCompactStyle=UNUM_SHORT) e.g. "23K", "45B"
    | NUM_DECIMAL_COMPACT_LONG -- ^ Decimal format expressed using compact notation (long form, corresponds to UNumberCompactStyle=UNUM_LONG) e.g. "23 thousand", "45 billion"
    | NUM_CURRENCY_STANDARD -- ^ Currency format with a currency symbol, e.g., "$1.00", using non-accounting style for negative values (e.g. minus sign). Overrides any style specified using -cf- key in locale.
    | NUM_FORMAT_STYLE_COUNT -- ^ One more than the highest normal UNumberFormatStyle value. Deprecated: ICU 58 The numeric value may change over time, see ICU ticket #12420.
    | NUM_DEFAULT -- ^ Default format.
    | NUM_IGNORE -- ^ Alias for NUM_PATTERN_DECIMAL.
      deriving (Eq, Show, Typeable)

type UNumberFormatStyle = CInt

toNFS :: NumberFormatStyle -> UNumberFormatStyle
toNFS (NUM_PATTERN_DECIMAL _) = #const UNUM_PATTERN_DECIMAL
toNFS NUM_DECIMAL = #const UNUM_DECIMAL
toNFS NUM_CURRENCY = #const UNUM_CURRENCY
toNFS NUM_PERCENT = #const UNUM_PERCENT
toNFS NUM_SCIENTIFIC = #const UNUM_SCIENTIFIC
toNFS NUM_SPELLOUT = #const UNUM_SPELLOUT
toNFS NUM_ORDINAL = #const UNUM_ORDINAL
toNFS NUM_DURATION = #const UNUM_DURATION
toNFS NUM_NUMBERING_SYSTEM = #const UNUM_NUMBERING_SYSTEM
toNFS (NUM_PATTERN_RULEBASED _) = #const UNUM_PATTERN_RULEBASED
toNFS NUM_CURRENCY_ISO = #const UNUM_CURRENCY_ISO
toNFS NUM_CURRENCY_PLURAL = #const UNUM_CURRENCY_PLURAL
toNFS NUM_CURRENCY_ACCOUNTING = #const UNUM_CURRENCY_ACCOUNTING
toNFS NUM_CASH_CURRENCY = #const UNUM_CASH_CURRENCY
toNFS NUM_DECIMAL_COMPACT_SHORT = #const UNUM_DECIMAL_COMPACT_SHORT
toNFS NUM_DECIMAL_COMPACT_LONG = #const UNUM_DECIMAL_COMPACT_LONG
toNFS NUM_CURRENCY_STANDARD = #const UNUM_CURRENCY_STANDARD
toNFS NUM_FORMAT_STYLE_COUNT = #const UNUM_FORMAT_STYLE_COUNT
toNFS NUM_DEFAULT = #const UNUM_DEFAULT
toNFS NUM_IGNORE = #const UNUM_IGNORE

-- | Create and return a new NumberFormat for formatting and parsing numbers.
--
-- A NumberFormat may be used to format numbers by calling unum_format, and
-- to parse numbers by calling unum_parse. The caller must call unum_close when
-- done to release resources used by this object.
numberFormatter :: NumberFormatStyle -- ^ The type of number format to open. If NUM_PATTERN_DECIMAL or NUM_PATTERN_RULEBASED is passed then the number format is opened using the given pattern, which must conform to the syntax described in DecimalFormat or RuleBasedNumberFormat, respectively.
        -> LocaleName -- ^ 	A locale identifier to use to determine formatting and parsing conventions, or NULL to use the default locale, e.g. "de_DE".
        -> NumberFormat
numberFormatter sty@(NUM_PATTERN_DECIMAL pattern) loc = numberFormatter' (toNFS sty) pattern loc
numberFormatter sty@(NUM_PATTERN_RULEBASED pattern) loc = numberFormatter' (toNFS sty) pattern loc
numberFormatter style loc = numberFormatter' (toNFS style) T.empty loc

numberFormatter' :: UNumberFormatStyle -- ^ The type of number format to open. If NUM_PATTERN_DECIMAL or NUM_PATTERN_RULEBASED is passed then the number format is opened using the given pattern, which must conform to the syntax described in DecimalFormat or RuleBasedNumberFormat, respectively.
        -> Text
        -> LocaleName -- ^ 	A locale identifier to use to determine formatting and parsing conventions, or NULL to use the default locale, e.g. "de_DE".
        -> NumberFormat
numberFormatter' style pattern loc =
    System.IO.Unsafe.unsafePerformIO $ fmap C $ wrap $
    useAsUCharPtr pattern $ \patternPtr patternLen ->
            withLocaleName loc $
                handleParseError (== u_PARSE_ERROR) . (unum_open style patternPtr (fromIntegral patternLen))

foreign import ccall unsafe "hs_text_icu.h __hs_unum_open" unum_open
    :: UNumberFormatStyle -> Ptr UChar -> Int32 -> CString -> Ptr UParseError  -> Ptr UErrorCode -> IO (Ptr UNumberFormat)

-- | Format an integer using a NumberFormat.
--
-- The integer will be formatted according to the UNumberFormat's locale.
class FormattableNumber n where
    formatNumber :: NumberFormat -- ^ The formatter to use.
                -> n -- ^ The number to format.
                -> Text

-- | Create a formatter and apply it in one step.
formatNumber' :: (FormattableNumber n)
            => NumberFormatStyle -- ^ The type of number format to open. If NUM_PATTERN_DECIMAL or NUM_PATTERN_RULEBASED is passed then the number format is opened using the given pattern, which must conform to the syntax described in DecimalFormat or RuleBasedNumberFormat, respectively.
            -> LocaleName -- ^ 	A locale identifier to use to determine formatting and parsing conventions, or NULL to use the default locale, e.g. "de_DE".
            -> n -- ^ The number to format.
            -> Text
formatNumber' style loc x = formatNumber (numberFormatter style loc) x

instance FormattableNumber Integer where
    formatNumber (C nf) x = numberFormatInt nf (fromIntegral x)

instance FormattableNumber Natural where
    formatNumber (C nf) x = numberFormatInt nf (fromIntegral x)

instance FormattableNumber Int where
    formatNumber (C nf) x = numberFormatInt nf x

instance FormattableNumber Double where
    formatNumber (C nf) x = numberFormatDouble nf x

instance FormattableNumber Float where
    formatNumber (C nf) x = numberFormatDouble nf (fromRational $ toRational x)

-- | Create a number format.
numberFormatInt :: MNumberFormat -> Int -> Text
numberFormatInt nf x = System.IO.Unsafe.unsafePerformIO $
  withNumberFormat nf $ \nptr ->
    handleOverflowError 100
        (\dptr dlen ec -> unum_formatInt64 nptr (fromIntegral x) dptr (fromIntegral dlen) ec)
        (\dptr dlen -> fromUCharPtr dptr (fromIntegral dlen))

-- | Format a number.
numberFormatDouble :: MNumberFormat -> Double -> Text
numberFormatDouble nf x = System.IO.Unsafe.unsafePerformIO $
  withNumberFormat nf $ \nptr ->
    handleOverflowError 100
        (\dptr dlen ec -> unum_formatDouble nptr (CDouble x) dptr (fromIntegral dlen) ec)
        (\dptr dlen -> fromUCharPtr dptr (fromIntegral dlen))

foreign import ccall unsafe "hs_text_icu.h __hs_unum_formatInt64" unum_formatInt64
    :: Ptr UNumberFormat -> Int -> Ptr UChar -> Int32 -> Ptr UErrorCode -> IO Int32
foreign import ccall unsafe "hs_text_icu.h __hs_unum_formatDouble" unum_formatDouble
    :: Ptr UNumberFormat -> CDouble -> Ptr UChar -> Int32 -> Ptr UErrorCode -> IO Int32

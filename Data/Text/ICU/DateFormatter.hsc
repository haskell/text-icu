{-# LANGUAGE EmptyDataDecls, BlockArguments, ImportQualifiedPost, RankNTypes, BangPatterns, ForeignFunctionInterface, RecordWildCards #-}
-- |
-- Module      : Data.Text.ICU.DateFormatter
-- Copyright   : (c) 2021 Torsten Kemps-Benedix
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Calendar formatter implemented as bindings to
-- the International Components for Unicode (ICU) libraries.

module Data.Text.ICU.DateFormatter
    (DateFormatter, FormatStyle(..), DateFormatSymbolType(..), standardDateFormatter, patternDateFormatter, dateSymbols, formatCalendar
    ) where

#include <unicode/udat.h>

import Control.Monad (forM)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Foreign (useAsPtr, fromPtr)
import Data.Text.ICU.Error.Internal (UErrorCode, handleError, handleOverflowError)
import Data.Text.ICU.Internal (LocaleName(..), UChar, withLocaleName)
import Data.Text.ICU.Calendar
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..), CDouble(..))
import Foreign.ForeignPtr (newForeignPtr, withForeignPtr, ForeignPtr)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr)
import Prelude hiding (last)
import System.IO.Unsafe (unsafePerformIO)

data FormatStyle =
       FullFormatStyle -- ^ Full style. 
       | LongFormatStyle -- ^ Long style.
       | MediumFormatStyle -- ^ Medium style.
       | ShortFormatStyle -- ^ Short style.
       | DefaultFormatStyle -- ^ Default style
       | RelativeFormatStyle -- ^ Relative style.
       | NoFormatStyle -- ^ No style.
  deriving (Eq, Enum, Show)

toUDateFormatStyle FullFormatStyle = #const UDAT_FULL
toUDateFormatStyle LongFormatStyle = #const UDAT_LONG
toUDateFormatStyle MediumFormatStyle = #const UDAT_MEDIUM
toUDateFormatStyle ShortFormatStyle = #const UDAT_SHORT
toUDateFormatStyle DefaultFormatStyle = #const UDAT_DEFAULT
toUDateFormatStyle RelativeFormatStyle = #const UDAT_RELATIVE
toUDateFormatStyle NoFormatStyle = #const UDAT_NONE

data DateFormatSymbolType =
        Eras -- ^  The era names, for example AD.
        | Months -- ^  The month names, for example February.
        | ShortMonths -- ^  The short month names, for example Feb.
        | Weekdays -- ^  The CLDR-style format "wide" weekday names, for example Monday.
        | ShortWeekdays -- ^  The CLDR-style format "abbreviated" (not "short") weekday names, for example "Mon." For the CLDR-style format "short" weekday names, use UDAT_SHORTER_WEEKDAYS.
        | AmPms -- ^  The AM/PM names, for example AM.
        | LocalizedChars -- ^  The localized characters.
        | EraNames -- ^  The long era names, for example Anno Domini.
        | NarrowMonths -- ^  The narrow month names, for example F.
        | NarrowWeekdays -- ^  The CLDR-style format "narrow" weekday names, for example "M".
        | StandaloneMonths -- ^  Standalone context versions of months.
        | StandaloneWeekdays -- ^  The CLDR-style stand-alone "wide" weekday names.
        | StandaoneShortWeekdays -- ^  The CLDR-style stand-alone "abbreviated" (not "short") weekday names. For the CLDR-style stand-alone "short" weekday names, use UDAT_STANDALONE_SHORTER_WEEKDAYS.
        | StandaloneNarrowWeekdays -- ^  The CLDR-style stand-alone "narrow" weekday names.
        | Quarters -- ^  The quarters, for example 1st Quarter.
        | ShortQuarters -- ^  The short quarter names, for example Q1.
        | StandaloneQuarters -- ^  Standalone context versions of quarters.
        | ShorterWeekdays -- ^  The CLDR-style short weekday names, e.g. "Su", Mo", etc. These are named "SHORTER" to contrast with the constants using SHORT above, which actually get the CLDR-style abbreviated versions of the corresponding names.
        | StandaloneShorterWeekdays -- ^  Standalone version of UDAT_SHORTER_WEEKDAYS.
        | CyclicYearsWide -- ^  Cyclic year names (only supported for some calendars, and only for FORMAT usage; udat_setSymbols not supported for UDAT_CYCLIC_YEARS_WIDE)
        | CyclicYearsAbbreviated -- ^  Cyclic year names (only supported for some calendars, and only for FORMAT usage)
        | CyclicYearsNarrow -- ^  Cyclic year names (only supported for some calendars, and only for FORMAT usage; udat_setSymbols not supported for UDAT_CYCLIC_YEARS_NARROW)
        | ZodiacNamesWide -- ^  Calendar zodiac names (only supported for some calendars, and only for FORMAT usage; udat_setSymbols not supported for UDAT_ZODIAC_NAMES_WIDE)
        | ZodiacNamesAbbreviated -- ^ Calendar zodiac names (only supported for some calendars, and only for FORMAT usage)
        | ZodiacNamesNarrow -- ^  Calendar zodiac names (only supported for some calendars, and only for FORMAT usage; udat_setSymbols not supported for UDAT_ZODIAC_NAMES_NARROW)

toUDateFormatSymbolType Eras = #const UDAT_ERAS
toUDateFormatSymbolType Months = #const UDAT_MONTHS
toUDateFormatSymbolType ShortMonths = #const UDAT_SHORT_MONTHS
toUDateFormatSymbolType Weekdays = #const UDAT_WEEKDAYS
toUDateFormatSymbolType ShortWeekdays = #const UDAT_SHORT_WEEKDAYS
toUDateFormatSymbolType AmPms = #const UDAT_AM_PMS
toUDateFormatSymbolType LocalizedChars = #const UDAT_LOCALIZED_CHARS
toUDateFormatSymbolType EraNames = #const UDAT_ERA_NAMES
toUDateFormatSymbolType NarrowMonths = #const UDAT_NARROW_MONTHS
toUDateFormatSymbolType NarrowWeekdays = #const UDAT_NARROW_WEEKDAYS
toUDateFormatSymbolType StandaloneMonths = #const UDAT_STANDALONE_MONTHS
toUDateFormatSymbolType StandaloneWeekdays = #const UDAT_STANDALONE_WEEKDAYS
toUDateFormatSymbolType StandaoneShortWeekdays = #const UDAT_STANDALONE_SHORT_WEEKDAYS
toUDateFormatSymbolType StandaloneNarrowWeekdays = #const UDAT_STANDALONE_NARROW_WEEKDAYS
toUDateFormatSymbolType Quarters = #const UDAT_QUARTERS
toUDateFormatSymbolType ShortQuarters = #const UDAT_SHORT_QUARTERS
toUDateFormatSymbolType StandaloneQuarters = #const UDAT_STANDALONE_QUARTERS
toUDateFormatSymbolType ShorterWeekdays = #const UDAT_SHORTER_WEEKDAYS
toUDateFormatSymbolType StandaloneShorterWeekdays = #const UDAT_STANDALONE_SHORTER_WEEKDAYS
toUDateFormatSymbolType CyclicYearsWide = #const UDAT_CYCLIC_YEARS_WIDE
toUDateFormatSymbolType CyclicYearsAbbreviated = #const UDAT_CYCLIC_YEARS_ABBREVIATED
toUDateFormatSymbolType CyclicYearsNarrow = #const UDAT_CYCLIC_YEARS_NARROW
toUDateFormatSymbolType ZodiacNamesWide = #const UDAT_ZODIAC_NAMES_WIDE
toUDateFormatSymbolType ZodiacNamesAbbreviated = #const UDAT_ZODIAC_NAMES_ABBREVIATED
toUDateFormatSymbolType ZodiacNamesNarrow = #const UDAT_ZODIAC_NAMES_NARROW

type UDateFormatStyle = CInt
type UFieldPosition = CInt
type UDateFormatSymbolType = CInt

data UDateFormat

newtype DateFormatter = DateFormatter (ForeignPtr UDateFormat)

-- | Create a new 'DateFormatter'.
--
-- >>> import Data.Text
-- >>> dfDe <- standardDateFormatter LongFormatStyle LongFormatStyle (Locale "de_DE") (pack "CET")
standardDateFormatter :: FormatStyle -> FormatStyle -> LocaleName -> Text -> IO DateFormatter
standardDateFormatter timeStyle dateStyle loc timeZoneId =
  withLocaleName loc $ \locale ->
    useAsPtr timeZoneId $ \tzPtr tzLen -> do
      df <- handleError $ udat_open (toUDateFormatStyle timeStyle) (toUDateFormatStyle dateStyle) locale tzPtr (fromIntegral tzLen) nullPtr (0 :: Int32)
      dfPtr <- newForeignPtr udat_close df
      pure $ DateFormatter dfPtr

-- | Create a new 'DateFormatter'.
patternDateFormatter :: Text -> LocaleName -> Text -> IO DateFormatter
patternDateFormatter pattern loc timeZoneId =
  withLocaleName loc $ \locale ->
    useAsPtr timeZoneId $ \tzPtr tzLen -> do
      useAsPtr pattern $ \patPtr patLen -> do
        df <- handleError $ udat_open (fromIntegral (#const UDAT_PATTERN)) (fromIntegral (#const UDAT_PATTERN)) locale tzPtr (fromIntegral tzLen) patPtr (fromIntegral patLen)
        dfPtr <- newForeignPtr udat_close df
        pure $ DateFormatter dfPtr

-- | Get relevant date related symbols, e.g. month and weekday names.
--
-- >>> import Data.Text
-- >>> dfDe <- standardDateFormatter LongFormatStyle LongFormatStyle (Locale "de_DE") (pack "CET")
-- >>> dateSymbols dfDe Months
-- ["Januar","Februar","M\228rz","April","Mai","Juni","Juli","August","September","Oktober","November","Dezember"]
-- >>> dfAt <- standardDateFormatter LongFormatStyle LongFormatStyle (Locale "de_AT") (pack "CET")
-- >>> dateSymbols dfAt Months
-- ["J\228nner","Februar","M\228rz","April","Mai","Juni","Juli","August","September","Oktober","November","Dezember"]
dateSymbols :: DateFormatter -> DateFormatSymbolType -> IO [Text]
dateSymbols (DateFormatter df) symType = do
  withForeignPtr df $ \dfPtr -> do
    n <- udat_countSymbols dfPtr (toUDateFormatSymbolType symType)
    syms <- forM [0..(n-1)] \i -> do
      handleOverflowError (fromIntegral (64 :: Int))
        (\dptr dlen -> udat_getSymbols dfPtr (toUDateFormatSymbolType symType) (fromIntegral i) dptr dlen)
        (\dptr dlen -> fromPtr (castPtr dptr) (fromIntegral dlen))
    pure $ filter (not . T.null) syms

-- >>> import Data.Text
-- >>> dfDe <- standardDateFormatter LongFormatStyle LongFormatStyle (Locale "de_DE") (pack "CET")
-- >>> c <- calendar (pack "CET") (Locale "de_DE") TraditionalCalendarType
-- >>> formatCalendar dfDe c
-- "13. Oktober 2021 um 12:44:09 GMT+2"
formatCalendar :: DateFormatter -> Calendar -> IO Text
formatCalendar (DateFormatter df) (Calendar cal) =
  withForeignPtr df $ \dfPtr -> do
    withForeignPtr cal $ \calPtr -> do
      handleOverflowError (fromIntegral (64 :: Int))
        (\dptr dlen -> udat_formatCalendar dfPtr calPtr dptr dlen nullPtr)
        (\dptr dlen -> fromPtr (castPtr dptr) (fromIntegral dlen))

{- 
UDateFormat *__hs_udat_open(UDateFormatStyle timeStyle, UDateFormatStyle dateStyle, const char *locale, 
  const UChar *tzID, int32_t tzIDLength, 
  const UChar *pattern, int32_t patternLength, 
  UErrorCode *status);
void __hs_udat_close(UDateFormat *format);
UDateFormat *__hs_udat_clone(const UDateFormat *fmt, UErrorCode *status);
int32_t __hs_udat_formatCalendar(const UDateFormat *format, UCalendar *calendar, UChar *result, int32_t capacity, UFieldPosition *position, UErrorCode *status);
int32_t __hs_udat_getSymbols(const UDateFormat *fmt, UDateFormatSymbolType type, int32_t symbolIndex, UChar *result, int32_t resultLength, UErrorCode *status);
int32_t __hs_udat_countSymbols(const UDateFormat *fmt, UDateFormatSymbolType type);
 -}

foreign import ccall unsafe "hs_text_icu.h __hs_udat_open" udat_open
    :: UDateFormatStyle -> UDateFormatStyle 
    -> CString 
    -> Ptr UChar -> Int32 
    -> Ptr UChar -> Int32 
    -> Ptr UErrorCode 
    -> IO (Ptr UDateFormat)
foreign import ccall unsafe "hs_text_icu.h &__hs_udat_close" udat_close
    :: FunPtr (Ptr UDateFormat -> IO ())
foreign import ccall unsafe "hs_text_icu.h __hs_udat_clone" udat_clone
    :: Ptr UDateFormat -> Ptr UErrorCode -> IO (Ptr UDateFormat)
foreign import ccall unsafe "hs_text_icu.h __hs_udat_formatCalendar" udat_formatCalendar
    :: Ptr UDateFormat 
    -> Ptr UCalendar 
    -> Ptr UChar -> Int32 
    -> Ptr UFieldPosition 
    -> Ptr UErrorCode 
    -> IO Int32
foreign import ccall unsafe "hs_text_icu.h __hs_udat_getSymbols" udat_getSymbols
    :: Ptr UDateFormat -> UDateFormatSymbolType -> Int32 -> Ptr UChar -> Int32 -> Ptr UErrorCode -> IO Int32
foreign import ccall unsafe "hs_text_icu.h __hs_udat_countSymbols" udat_countSymbols
    :: Ptr UDateFormat -> UDateFormatSymbolType -> IO Int32

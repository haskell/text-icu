{-# LANGUAGE CPP, DeriveDataTypeable, ForeignFunctionInterface,
    GeneralizedNewtypeDeriving,EmptyDataDecls #-}
-- |DateTimeFormatting module consists of functions that convert dates and times from their 
-- internal representations to textual form and back again in a language-independent manner. 
-- Converting from the internal representation (milliseconds since midnight, January 1, 1970) to 
-- text is known as \"formatting,\" and converting from text to millis is known as \"parsing.\" 
-- We currently define only one concrete structure 'DateFormat', which can handle pretty much all 
-- normal date formatting and parsing actions.
-- 
-- 'DateTimeFormatting' helps you to format and parse dates for any locale. Your code can be 
-- completely independent of the locale conventions for months, days of the week, or even the 
-- calendar format: lunar vs. solar.
--
-- Example:
--
-- > let (Right df) = openDateFormat dfsLong dfsLong "de" (pack "GMT")
-- > dt <- getNow
-- > formatDate df dt
-- 
-- This gives: @Right \"1. April 2009 20:40:05 GMT+00:00\"@ using the locale \"fr\" instead would 
-- yield: @Right \"1 avril 2009 20:40:05 UTC+00:00\"@ and \"ru\" produces
-- @Right \"1 &#1072;&#1087;&#1088;&#1077;&#1083;&#1103; &#2009; &#1075;. 20:40:05 GMT+00:00\"@.
module Data.Text.ICU.DateTimeFormatting
    (DateFormatStyle(..),dfsFull,dfsLong,dfsMedium,dfsShort,dfsDefault,dfsRelative,
     dfsFullRelative,dfsLongRelative,dfsMediumRelative,dfsShortRelative,dfsNone,dfsIgnore,
     DateFormatField(..),dffEraField,dffYearField,dffMonthField,dffDateField,dffHourOfDay1Field,
     dffFourOfDay0Field,dffMinuteField,dffSecondField,dffFractionalSecondField,dffDayOfWeekField,
     dffDayOfYearField,dffDayOfweekInMonthField,dffWeekOfYearField,dffWeekOfMonthField,
     dffAmPmField,dffHour1Field,dffHour0Field,dffTimeZoneField,dffYearWOYField,dffDOWLocalField,
     dffExtendedYearField,dffJulianDayField,dffMilliSecondsInDayField,dffTimezoneRFCField,
     dffTimeZoneGenericField,dffStandAloneDayField,dffStandAloneMonthField,dffQuarterField,
     dffStandAloneQuarterField,dffTimeZoneSpecialField,dffFieldCount,
     DateFormat,openDateFormat,formatDate
    )
    where

#include <unicode/udat.h>

import Control.Exception (throw)
import Control.Monad (when)
import Data.Text (Text)
import Data.Text.Foreign (useAsPtr,fromPtr)
import Data.Text.ICU.Calendars (Date,fromDate,UDate)
import Data.Text.ICU.Error.Internal (UErrorCode,isFailure,withError)
import Data.Text.ICU.FieldPosition (FieldPosition)
import Data.Text.ICU.Internal (UChar)
import Data.Typeable (Typeable)
import Data.Int (Int32)
import Data.Word (Word32)
import Foreign.C.String (CString,withCAString)
import Foreign.ForeignPtr (ForeignPtr,newForeignPtr,withForeignPtr)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr,nullPtr,FunPtr)
import System.IO.Unsafe (unsafePerformIO)

-- |The possible date/time format styles.
newtype DateFormatStyle = DateFormatStyle {
      fromDateFormatStyle :: Word32
    } deriving (Eq,Typeable,Show)

#{enum DateFormatStyle,DateFormatStyle,
       dfsFull = UDAT_FULL, 
       dfsLong = UDAT_LONG, 
       dfsMedium = UDAT_MEDIUM, 
       dfsShort = UDAT_SHORT, 
       dfsDefault = UDAT_DEFAULT, 
       dfsRelative = UDAT_RELATIVE, 
       dfsFullRelative = UDAT_FULL_RELATIVE,
       dfsLongRelative = UDAT_LONG_RELATIVE,
       dfsMediumRelative = UDAT_MEDIUM_RELATIVE,
       dfsShortRelative = UDAT_SHORT_RELATIVE,
       dfsNone = UDAT_NONE, 
       dfsIgnore = UDAT_IGNORE}

-- |'FieldPosition' selectors for format fields defined by 'DateFormat'.
newtype DateFormatField = DateFormatField {
      fromDateFormatField :: Word32
    } deriving (Eq,Typeable,Show)

#{enum DateFormatField,DateFormatField,
       dffEraField = UDAT_ERA_FIELD,
       dffYearField = UDAT_YEAR_FIELD,
       dffMonthField = UDAT_MONTH_FIELD,
       dffDateField = UDAT_DATE_FIELD,
       dffHourOfDay1Field = UDAT_HOUR_OF_DAY1_FIELD,
       dffFourOfDay0Field = UDAT_HOUR_OF_DAY0_FIELD,
       dffMinuteField = UDAT_MINUTE_FIELD,
       dffSecondField = UDAT_SECOND_FIELD,
       dffFractionalSecondField = UDAT_FRACTIONAL_SECOND_FIELD,
       dffDayOfWeekField = UDAT_DAY_OF_WEEK_FIELD,
       dffDayOfYearField = UDAT_DAY_OF_YEAR_FIELD,
       dffDayOfweekInMonthField = UDAT_DAY_OF_WEEK_IN_MONTH_FIELD,
       dffWeekOfYearField = UDAT_WEEK_OF_YEAR_FIELD,
       dffWeekOfMonthField = UDAT_WEEK_OF_MONTH_FIELD,
       dffAmPmField = UDAT_AM_PM_FIELD,
       dffHour1Field = UDAT_HOUR1_FIELD,
       dffHour0Field = UDAT_HOUR0_FIELD,
       dffTimeZoneField = UDAT_TIMEZONE_FIELD,
       dffYearWOYField = UDAT_YEAR_WOY_FIELD,
       dffDOWLocalField = UDAT_DOW_LOCAL_FIELD,
       dffExtendedYearField = UDAT_EXTENDED_YEAR_FIELD,
       dffJulianDayField = UDAT_JULIAN_DAY_FIELD,
       dffMilliSecondsInDayField = UDAT_MILLISECONDS_IN_DAY_FIELD,
       dffTimezoneRFCField = UDAT_TIMEZONE_RFC_FIELD,
       dffTimeZoneGenericField = UDAT_TIMEZONE_GENERIC_FIELD,
       dffStandAloneDayField = UDAT_STANDALONE_DAY_FIELD,
       dffStandAloneMonthField = UDAT_STANDALONE_MONTH_FIELD,
       dffQuarterField = UDAT_QUARTER_FIELD,
       dffStandAloneQuarterField = UDAT_STANDALONE_QUARTER_FIELD,
       dffTimeZoneSpecialField = UDAT_TIMEZONE_SPECIAL_FIELD}

dffFieldCount :: Int
dffFieldCount = #const UDAT_FIELD_COUNT

data UDateFormat

-- |A date formatter.
type DateFormat = ForeignPtr UDateFormat

-- |Open a new 'DateFormat' for formatting and parsing dates and times. A 'DateFormat' may be 
-- used to format dates in calls to 'formatDate', and to parse dates in calls to 'parseDate'.
openDateFormat :: DateFormatStyle -> DateFormatStyle -> String -> Text -> DateFormat
openDateFormat timeStyle dateStyle locale tzID = unsafePerformIO $ do
  withCAString locale $ \locale' -> do
    useAsPtr tzID $ \tzID' lTzID -> do
      (err,df) <- withError $ udat_open (fromDateFormatStyle timeStyle) (fromDateFormatStyle dateStyle) locale' tzID' (fromIntegral lTzID) nullPtr 0
      when (isFailure err) (throw err)
      newForeignPtr udat_close df

-- |Format a date using a 'DateFormat'. The date will be formatted using the conventions 
-- specified in 'openDateFormat'.
formatDate :: DateFormat -> Date -> Text
formatDate df date = unsafePerformIO $ do
  allocaArray 256 $ \dptr -> do
    withForeignPtr df $ \pdf -> do
      (err,l) <- withError $ udat_format pdf (fromRational . toRational . fromDate $ date) dptr 256 nullPtr
      when (isFailure err) (throw err)
      fromPtr dptr (fromIntegral l)

foreign import ccall unsafe "unicode/unorm.h udat_open_4_0" udat_open
    :: Word32 -> Word32 -> CString -> Ptr UChar -> Int32 -> Ptr UChar -> Int32 -> Ptr UErrorCode -> IO (Ptr UDateFormat)
foreign import ccall "unicode/udat.h &udat_close_4_0" udat_close
    :: FunPtr (Ptr UDateFormat -> IO ())
foreign import ccall unsafe "unicode/udat.h udat_format_4_0" udat_format
    :: Ptr UDateFormat -> UDate -> Ptr UChar -> Int32 -> Ptr FieldPosition -> Ptr UErrorCode -> IO Int32

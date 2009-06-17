{-# LANGUAGE CPP, DeriveDataTypeable, EmptyDataDecls, ForeignFunctionInterface,
    GeneralizedNewtypeDeriving, OverloadedStrings, StandaloneDeriving #-}
-- | The Calendars module is used for converting between a 'Date' and a
-- set of 'Int' fields such as 'year', 'month', 'day', 'hour', and so
-- on. A Date represents a specific instant in time with millisecond
-- precision. See 'Date' for information.
--
-- Types of 'Calendar' interpret a 'Date' according to the rules of a
-- specific calendar system. The 'CalendarType' provides the types
-- 'traditional' and 'gregorian'.
--
-- Like other locale-sensitive APIs this module provides a function
-- 'openCalendar', which returns a 'Calendar' whose time fields have
-- been initialized with the current date and time. We need to specify
-- the type of calendar to be opened and the time zone id.  A
-- UCalendar object can produce all the time field values needed to
-- implement the date-time formatting for a particular language and
-- calendar style (for example, Japanese-Gregorian,
-- Japanese-Traditional).
--
-- When computing a 'Date' from time fields, two special circumstances
-- may arise: there may be insufficient information to compute the
-- 'Date' (such as only year and month but no day in the month), or
-- there may be inconsistent information (such as \"Tuesday, July 15,
-- 1996\" -- July 15, 1996 is actually a Monday).
--
-- * /Insufficient information./ The calendar will use default information to
-- specify the missing fields. This may vary by calendar; for the
-- Gregorian calendar, the default for a field is the same as that of
-- the start of the epoch: i.e., year = 1970, month = January, date =
-- 1, etc.
--
-- * /Inconsistent information./ If fields conflict, the calendar will give
-- preference to fields set more recently. For example, when
-- determining the day, the calendar will look for one of the
-- following combinations of fields. The most recent combination, as
-- determined by the most recently set single field, will be used.
--
-- Note: for some non-Gregorian calendars, different fields may be
-- necessary for complete disambiguation. For example, a full
-- specification of the historial Arabic astronomical calendar
-- requires year, month, day-of-month and day-of-week in some cases.
--
-- /Note:/ There are certain possible ambiguities in interpretation of
-- certain singular times, which are resolved in the following ways:
--
-- 24:00:00 \"belongs\" to the following day. That is, 23:59 on Dec
-- 31, 1969 < 24:00 on Jan 1, 1970 < 24:01:00 on Jan 1, 1970 Although
-- historically not precise, midnight also belongs to \"am\", and noon
-- belongs to \"pm\", so on the same day, 12:00 am (midnight) < 12:01
-- am, and 12:00 pm (noon) < 12:01 pm The date or time format strings
-- are not part of the definition of a calendar, as those must be
-- modifiable or overridable by the user at runtime. Use DateFormat to
-- format dates.
--
-- Calendars provides an API for field \"rolling\", where fields can
-- be incremented or decremented, but wrap around. For example,
-- rolling the month up in the date December 12, 1996 results in
-- January 12, 1996.
--
-- Calendars also provides a date arithmetic function for adding the
-- specified (signed) amount of time to a particular time field. For
-- example, subtracting 5 days from the date September 12, 1996
-- results in September 7, 1996.
module Data.Text.ICU.Calendar
    (
     -- * Calendar metadata
     -- ** Types
     CalendarType(..),traditional,gregorian,
     -- ** Date fields
     CalendarDateFields(..),era,year,month,weekOfYear,weekOfMonth,date,dayOfYear,dayOfWeek,
     dayOfWeekInMonth,am_pm,hour,hourOfDay,minute,second,milliSecond,zoneOffset,dstOffset,
     yearWOY,dowLocal,extendedYear,julianDay,millisecondsInDay,isLeapMonth,dayOfMonth,
     -- ** Days of week
     DayOfWeek,sunday,monday,tuesday,wednesday,thursday,friday,saturday,
     -- ** Months
     Month,january,february,march,april,may,june,july,august,september,
     october,november,december,undecimber,
     -- ** AM and PM
     CalendarAMPMs(..),am,pm,
     -- ** Display names and types
     CalendarDisplayNameType(..),standard,shortStandard,dst,shortDst,
     -- ** Attributes
     CalendarAttribute(..),lenient,firstDayOfWeek,minimalDaysInFirstWeek,
     -- ** Limits
     CalendarLimitType(..),cltMin,cltMax,greatestMinimum,leastMaximum,actualMinimum,actualMaximum,
     -- * Date
     Date(..),UDate,getNow,
     -- * Calendar creation and manipulation
     Calendar,calendarType,calendarLocale,calendarZoneID,openCalendar,makeCalendar,makeCleanCalendar,cloneCalendar,
     getMillis,setMillis,
     getCalendar,setCalendar,updateCalendar,clearCalendar,
     -- * Time zones
     openTimeZones,timeZones,openCountryTimeZones,countryTimeZones,getDefaultTimeZone,
     setDefaultTimeZone,getCanonicalTimeZoneID,
     -- * Equivalence
     equivalent
    )
    where

#include <unicode/ucal.h>

import Control.Applicative ((<$>))
import Data.Data (Data(..))
import Data.Ord (Ord(..))
import Data.Text (Text)
import Data.Text.Foreign (useAsPtr,fromPtr)
import Data.Text.ICU.Error.Internal (UErrorCode,handleError)
import Data.Text.ICU.Internal (UChar)
import Data.Text.ICU.Enumeration (Enumeration,UEnumeration,enumerationFinalizer,enumerationTexts)
import Data.Typeable (Typeable)
import Data.Int (Int32)
import Data.Word (Word32,Word8)
import Foreign.C.String (CString,withCAString)
import Foreign.C.Types (CDouble)
import Foreign.ForeignPtr (ForeignPtr,newForeignPtr,withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr,FunPtr)
import Foreign.Storable (peek)
import System.IO.Unsafe (unsafePerformIO)

-- | Possible types of 'Calendar's.
-- There is a 'traditional' calendar for the locale and
-- the Gregorian calendar style is created with 'gregorian'.
newtype CalendarType = CalendarType {
      fromCalendarType :: Word32
    }
    deriving (Eq,Typeable,Data,Show)

#{enum CalendarType,CalendarType,
  traditional = UCAL_TRADITIONAL,
  gregorian = UCAL_GREGORIAN}

-- | Possible fields in a 'Calendar':
--
-- * 'era': Field number indicating the era, e.g., AD or BC in the
--   Gregorian (Julian) calendar.  This is a calendar-specific value.
--
-- * 'year': Field number indicating the year. This is a
--   calendar-specific value.
--
-- * 'month': Field number indicating the month. This is a
--   calendar-specific value. The first month of the year is January;
--   the last depends on the number of months in a year.
--
-- * 'weekOfYear': Field number indicating the week number within the
--   current year.  The first week of the year, as defined by
--   'firstDayOfWeek' and 'minimalDaysInFirstWeek' attributes, has
--   value 1. Subclasses define the value of 'weekOfYear' for days
--   before the first week of the year.
--
-- * 'weekOfMonth': Field number indicating the week number within the
--   current month.  The first week of the month, as defined by
--   'firstDayOfWeek' and 'minimalDaysInFirstWeek' attributes, has
--   value 1. Subclasses define the value of 'weekOfMonth' for days
--   before the first week of the month.
--
-- * 'date': Field number indicating the day of the month. This is a
--   synonym for 'dayOfMonth'.  The first day of the month has value
--   1.
--
-- * 'dayOfYear': Field number indicating the day number within the
--   current year. The first day of the year has value 1.
--
-- * 'dayOfWeek': Field number indicating the day of the week. This
--   field takes values 'sunday', 'monday', 'tuesday etc.
--
-- * 'dayOfWeekInMonth': Field number indicating the ordinal number of
--   the day of the week within the current month. Together with the
--   'dayOfWeek' field, this uniquely specifies a day within a
--   month. Unlike 'weekOfMonth' and 'weekOfYear', this field's value
--   does not depend on getFirstDayOfWeek or
--   getMinimalDaysInFirstWeek. Values of 'dayOfMonth' 1 through 7
--   always correspond to 'dayOfWeekInMonth' 1; 8 through 15
--   correspond to 'dayOfWeekInMonth' 2, and so on. 'dayOfWeekInMonth'
--   0 indicates the week before 'dayOfWeekInMonth' 1. Negative values
--   count back from the end of the month, so the last Sunday of a
--   month is specified as 'dayOfWeek' = 'sunday', 'dayOfWeekInMonth'@
--   = -1@. Because negative values count backward they will usually
--   be aligned differently within the month than positive values. For
--   example, if a month has 31 days, 'dayOfWeekInMonth' -1 will
--   overlap 'dayOfWeekInMonth' 5 and the end of 4.
--
-- * 'am_pm': Field number indicating whether the HOUR is before or
--   after noon. E.g., at 10:04:15.250 PM the am_pm is PM.
--
-- * 'hour': Field number indicating the hour of the morning or
--   afternoon. 'hour' is used for the 12-hour clock. E.g., at
--   10:04:15.250 PM the 'hour' is 10.
--
-- * 'hourOfDay': Field number indicating the hour of the
--   day. 'hourOfDay' is used for the 24-hour clock. E.g., at
--   10:04:15.250 PM the 'hourOfDay' is 22.
--
-- * 'minute': Field number indicating the minute within the
--   hour. E.g., at 10:04:15.250 PM the 'minute' is 4.
--
-- * 'second': Field number indicating the second within the
--   minute. E.g., at 10:04:15.250 PM the 'second' is 15.
--
-- * 'milliSecond': Field number indicating the millisecond within the
--   second.
--
-- * 'zoneOffset': Field number indicating the raw offset from GMT in
--   milliseconds.
--
-- * 'dstOffset': Field number indicating the daylight savings offset
--   in milliseconds
--
-- * 'yearWOY': Field number indicating the extended year
--   corresponding to the 'weekOfYear' field.  This may be one greater
--   or less than the value of 'extendedYear'.
--
-- * 'dowLocal': Field number indicating the localized day of
--   week. This will be a value from 1 to 7 inclusive, with 1 being
--   the localized first day of the week.
--
-- * 'extendedYear': Year of this calendar system, encompassing all
--   supra-year fields. For example, in Gregorian/Julian calendars,
--   positive extended year values indicate years AD, 1 BC = 0
--   extended, 2 BC = -1 extended, and so on.
--
-- * 'julianDay': Field number indicating the modified Julian day
--   number. This is different from the conventional Julian day number
--   in two regards. First, it demarcates days at local zone midnight,
--   rather than noon GMT. Second, it is a local number; that is, it
--   depends on the local time zone. It can be thought of as a single
--   number that encompasses all the date-related fields.
--
-- * 'milliSecondsInDay': Ranges from 0 to 23:59:59.999 (regardless of
--   DST). This field behaves exactly like a composite of all
--   time-related fields, not including the zone fields. As such, it
--   also reflects discontinuities of those fields on DST transition
--   days. On a day of DST onset, it will jump forward. On a day of
--   DST cessation, it will jump backward. This reflects the fact that
--   it must be combined with the DST_OFFSET field to obtain a unique
--   local time value.
--
-- * 'isLeapMonth': Whether or not the current month is a leap month
--   (0 or 1).
--
-- * 'dayOfMonth': Field number indicating the day of the month. This
--   is a synonym for 'date'.  The first day of the month has value 1.
newtype CalendarDateFields = CalendarDateFields {
      fromCalendarDateFields :: Word32
    }
    deriving (Eq,Typeable,Data,Show)

#{enum CalendarDateFields,CalendarDateFields,
       era = UCAL_ERA,
       year = UCAL_YEAR,
       month = UCAL_MONTH,
       weekOfYear = UCAL_WEEK_OF_YEAR,
       weekOfMonth = UCAL_WEEK_OF_MONTH,
       date = UCAL_DATE,
       dayOfYear = UCAL_DAY_OF_YEAR,
       dayOfWeek = UCAL_DAY_OF_WEEK,
       dayOfWeekInMonth = UCAL_DAY_OF_WEEK_IN_MONTH,
       am_pm = UCAL_AM_PM,
       hour = UCAL_HOUR,
       hourOfDay = UCAL_HOUR_OF_DAY,
       minute = UCAL_MINUTE,
       second = UCAL_SECOND,
       milliSecond = UCAL_MILLISECOND,
       zoneOffset = UCAL_ZONE_OFFSET,
       dstOffset = UCAL_DST_OFFSET,
       yearWOY = UCAL_YEAR_WOY,
       dowLocal = UCAL_DOW_LOCAL,
       extendedYear = UCAL_EXTENDED_YEAR,
       julianDay = UCAL_JULIAN_DAY,
       millisecondsInDay = UCAL_MILLISECONDS_IN_DAY,
       isLeapMonth = UCAL_IS_LEAP_MONTH}

dayOfMonth :: CalendarDateFields
dayOfMonth = CalendarDateFields (#const UCAL_DATE)

newtype DayOfWeek = DayOfWeek Word32
    deriving (Eq,Enum,Typeable,Data)

#{enum DayOfWeek,DayOfWeek,
       sunday = UCAL_SUNDAY,
       monday = UCAL_MONDAY,
       tuesday = UCAL_TUESDAY,
       wednesday = UCAL_WEDNESDAY,
       thursday = UCAL_THURSDAY,
       friday = UCAL_FRIDAY,
       saturday = UCAL_SATURDAY}

showDayOfWeek :: DayOfWeek -> String
showDayOfWeek (DayOfWeek #const UCAL_SUNDAY) = "sunday"
showDayOfWeek (DayOfWeek #const UCAL_MONDAY) = "monday"
showDayOfWeek (DayOfWeek #const UCAL_TUESDAY) = "tuesday"
showDayOfWeek (DayOfWeek #const UCAL_WEDNESDAY) = "wednesday"
showDayOfWeek (DayOfWeek #const UCAL_THURSDAY) = "thursday"
showDayOfWeek (DayOfWeek #const UCAL_FRIDAY) = "friday"
showDayOfWeek (DayOfWeek #const UCAL_SATURDAY) = "saturday"
showDayOfWeek _ = error "showDayOfWeek: impossible"

instance Show DayOfWeek where
    show = showDayOfWeek

newtype Month = Month Word32
    deriving (Eq,Typeable,Data,Show)

#{enum Month,Month,
       january = UCAL_JANUARY,
       february = UCAL_FEBRUARY,
       march = UCAL_MARCH,
       april = UCAL_APRIL,
       may = UCAL_MAY,
       june = UCAL_JUNE,
       july = UCAL_JULY,
       august = UCAL_AUGUST,
       september = UCAL_SEPTEMBER,
       october = UCAL_OCTOBER,
       november = UCAL_NOVEMBER,
       december = UCAL_DECEMBER,
       undecimber = UCAL_UNDECIMBER}

newtype CalendarAMPMs = CalendarAMPMs {
      fromCalendarAMPMs :: Word32
    }
    deriving (Eq,Typeable,Data,Show)

#{enum CalendarAMPMs,CalendarAMPMs,
       am = UCAL_AM,
       pm = UCAL_PM}

newtype CalendarDisplayNameType = CalendarDisplayNameType {
      fromCalendarDisplayNameType :: Word32
    }
    deriving (Eq,Typeable,Data,Show)

#{enum CalendarDisplayNameType,CalendarDisplayNameType,
       standard = UCAL_STANDARD,
       shortStandard = UCAL_SHORT_STANDARD,
       dst = UCAL_DST,
       shortDst = UCAL_SHORT_DST}

newtype CalendarAttribute = CalendarAttribute {
      fromCalendarAttribute :: Word32
    }
    deriving (Eq,Typeable,Data,Show)

#{enum CalendarAttribute,CalendarAttribute,
       lenient = UCAL_LENIENT,
       firstDayOfWeek = UCAL_FIRST_DAY_OF_WEEK,
       minimalDaysInFirstWeek = UCAL_MINIMAL_DAYS_IN_FIRST_WEEK}

newtype CalendarLimitType = CalendarLimitType {
      fromCalendarLimitType :: Word32
    }
    deriving (Eq,Typeable,Data,Show)

#{enum CalendarLimitType,CalendarLimitType,
       cltMin = UCAL_MINIMUM,
       cltMax = UCAL_MAXIMUM,
       greatestMinimum = UCAL_GREATEST_MINIMUM,
       leastMaximum = UCAL_LEAST_MAXIMUM,
       actualMinimum = UCAL_ACTUAL_MINIMUM,
       actualMaximum = UCAL_ACTUAL_MAXIMUM}

data UCalendar deriving Typeable
data Calendar = Calendar {
      calendarType :: CalendarType,
      calendarLocale :: String,
      calendarZoneID :: Text,
      uCalendar :: ForeignPtr UCalendar
    }
                deriving (Typeable)

showCalendar :: Calendar -> IO String
showCalendar cal = do
  era' <- getCalendar cal era :: IO Int
  year' <- getCalendar cal year :: IO Int
  month' <- getCalendar cal month :: IO Int
  date' <- getCalendar cal date :: IO Int
  hourOfDay' <- getCalendar cal hourOfDay :: IO Int
  minute' <- getCalendar cal minute :: IO Int
  second' <- getCalendar cal second :: IO Int
  milliSecond' <- getCalendar cal milliSecond :: IO Int
  zoneOffset' <- getCalendar cal zoneOffset :: IO Int
  dstOffset' <- getCalendar cal dstOffset :: IO Int
  return $ "Calendar {"
                                 ++"era="++show era'
                                 ++",year="++show year'
                                 ++",month="++show month'
                                 ++",day="++show date'
                                 ++",hour="++show hourOfDay'
                                 ++",minute="++show minute'
                                 ++",second="++show second'
                                 ++",milliSecond="++show milliSecond'
                                 ++",zoneOffset="++show zoneOffset'
                                 ++",dstOffset="++show dstOffset'
                                 ++"}"

instance Show Calendar where
    show = unsafePerformIO . showCalendar

type UDate = CDouble
data Date = Date {fromDate :: Double} deriving (Eq,Ord,Show,Typeable,Data)

-- | Get the current date and time. The value returned is represented as
-- milliseconds from the epoch.
getNow :: IO Date
getNow = Date . fromRational . toRational <$> ucal_getNow

-- | Open a 'Calendar'. A 'Calendar' may be used to convert a
-- millisecond value to a year, month, and day.
openCalendar :: Text      -- ^ The desired time zone ID. If empty, use the default time zone.
             -> String          -- ^ The desired locale.
             -> CalendarType    -- ^ The type of calendar to open.
             -> IO Calendar
openCalendar z loc t = do
  useAsPtr z $ \zoneID' l -> do
    withCAString loc $ \locale' -> do
      cal <- handleError $ ucal_open zoneID' (fromIntegral l) locale' t
      Calendar t loc z <$> newForeignPtr ucal_close cal

makeCalendar :: Text -> String -> CalendarType -> Date -> Calendar
makeCalendar zid loc t ms = unsafePerformIO $ do
  c <- openCalendar zid loc t
  setMillis c ms
  return c

makeCleanCalendar :: Calendar
makeCleanCalendar = unsafePerformIO $ do
  c <- openCalendar "" "" gregorian
  clearCalendar c
  return c

-- | Open a copy of a 'Calendar'. This function performs a deep copy.
cloneCalendar :: Calendar -> IO Calendar
cloneCalendar (Calendar t loc mz ucal) = do
  withForeignPtr ucal $ \ucal' -> do
    newCal <- handleError $ ucal_clone ucal'
    Calendar t loc mz <$> newForeignPtr ucal_close newCal

-- | Get a 'Calendar'\'s current time in millis as a 'Date'. The time
-- is represented as milliseconds from the epoch.
getMillis :: Calendar -> IO Date
getMillis cal =
  withForeignPtr (uCalendar cal) $
    fmap (Date . fromRational . toRational) . handleError . ucal_getMillis

-- | Set a 'Calendar'\'s current time in millis from a 'Date'. The
-- time is represented as milliseconds from the epoch.
setMillis :: Calendar -> Date -> IO ()
setMillis cal d = do
  withForeignPtr (uCalendar cal) $ \cal' ->
    handleError $ ucal_setMillis cal' (fromRational (toRational (fromDate d)))

-- | Get the value of a field in a 'Calendar'. All fields are
-- represented as 32-bit integers.
getCalendar :: (Integral a) => Calendar -> CalendarDateFields -> IO a
getCalendar cal field =
  withForeignPtr (uCalendar cal) $ \cal' ->
    fmap fromIntegral . handleError $ ucal_get cal' (fromCalendarDateFields field)

-- | Set the value of a field in a Calendar. All fields are
-- represented as 32-bit integers.
updateCalendar :: (Integral a) => Calendar -> CalendarDateFields -> a -> IO ()
updateCalendar cal field x = do
  withForeignPtr (uCalendar cal) $ \cal' -> do
    ucal_set cal' (fromCalendarDateFields field) (fromIntegral x)

-- | Clear all fields in a 'Calendar'.
clearCalendar :: Calendar -> IO ()
clearCalendar cal = withForeignPtr (uCalendar cal) ucal_clear

-- | Set the value of a field in a Calendar. All fields are
-- represented as 32-bit integers. The 'Calendar' is cloned before the
-- set operation.
setCalendar :: (Integral a) => Calendar -> CalendarDateFields -> a -> Calendar
setCalendar cal field x = unsafePerformIO $ do
  cal' <- cloneCalendar cal
  updateCalendar cal' field x
  return cal'

-- | Create an 'Enumeration' over all time zones.
openTimeZones :: IO Enumeration
openTimeZones = handleError ucal_openTimeZones >>= enumerationFinalizer

-- | Return a list of all time zones.
timeZones :: [Text]
timeZones = unsafePerformIO $ openTimeZones >>= enumerationTexts

-- | Create an 'Enumeration' over all time zones for a country. The
-- country is specified as String like e.g. \"de\" for Germany or
-- \"us\" for the US.
openCountryTimeZones :: String -> IO Enumeration
openCountryTimeZones ctry = withCAString ctry $ \ctry' ->
   handleError (ucal_openCountryTimeZones ctry') >>= enumerationFinalizer

-- | Create a list of all time zones for a country. The country is
-- specified as String like e.g.  \"de\" for Germany or \"us\" for the
-- US.
countryTimeZones :: String -> [Text]
countryTimeZones ctry = unsafePerformIO $
  openCountryTimeZones ctry >>= enumerationTexts

-- | Return the default time zone. The default is determined initially
-- by querying the host operating system. It may be changed with
-- 'setDefaultTimeZone'.
getDefaultTimeZone :: IO Text
getDefaultTimeZone = do
  allocaArray 256 $ \pt -> do
    n <- handleError $ ucal_getDefaultTimeZone pt 256
    fromPtr pt (fromIntegral n)

-- | Set the default time zone.
setDefaultTimeZone :: Text -> IO ()
setDefaultTimeZone tz =
  useAsPtr tz $ \tz' _ -> handleError $ ucal_setDefaultTimeZone tz'

-- | Return the canonical system timezone ID or the normalized custom
-- time zone ID for the given time zone ID.
getCanonicalTimeZoneID :: Text -> (Text,Bool)
getCanonicalTimeZoneID tz = unsafePerformIO $ do
  useAsPtr tz $ \pstz lstz -> do
    allocaArray 256 $ \pdtz -> do
      alloca $ \pIsSystemID -> do
        n <- handleError $ ucal_getCanonicalTimeZoneID pstz (fromIntegral lstz) pdtz 256 pIsSystemID
        dtz <- fromPtr pdtz (fromIntegral n)
        isSystemID <- peek pIsSystemID
        return (dtz,isSystemID/=0)

equivalent :: Calendar -> Calendar -> IO Bool
equivalent cal1 cal2 =
  withForeignPtr (uCalendar cal1) $ \cal1' ->
    withForeignPtr (uCalendar cal2) $ \cal2' ->
      (/=0) <$> ucal_equivalentTo cal1' cal2'

type UBool = Word8

foreign import ccall unsafe "unicode/ucal.h ucal_getNow_4_0" ucal_getNow
    :: IO UDate
foreign import ccall unsafe "unicode/ucal.h ucal_open_4_0" ucal_open
    :: Ptr UChar -> Int32 -> CString -> CalendarType -> Ptr UErrorCode -> IO (Ptr UCalendar)
foreign import ccall "unicode/ucal.h &ucal_close_4_0" ucal_close
    :: FunPtr (Ptr UCalendar -> IO ())
foreign import ccall unsafe "unicode/ucal.h ucal_getMillis_4_0" ucal_getMillis
    :: Ptr UCalendar -> Ptr UErrorCode -> IO UDate
foreign import ccall unsafe "unicode/ucal.h ucal_setMillis_4_0" ucal_setMillis
    :: Ptr UCalendar -> UDate -> Ptr UErrorCode -> IO ()
foreign import ccall unsafe "unicode/ucal.h ucal_get_4_0" ucal_get
    :: Ptr UCalendar -> Word32 -> Ptr UErrorCode -> IO Int32
foreign import ccall unsafe "unicode/ucal.h ucal_set_4_0" ucal_set
    :: Ptr UCalendar -> Word32 -> Int32 -> IO ()
foreign import ccall unsafe "unicode/ucal.h ucal_clone_4_0" ucal_clone
    :: Ptr UCalendar -> Ptr UErrorCode -> IO (Ptr UCalendar)
foreign import ccall unsafe "unicode/ucal.h ucal_openTimeZones_4_0" ucal_openTimeZones
    :: Ptr UErrorCode -> IO (Ptr UEnumeration)
foreign import ccall unsafe "unicode/ucal.h ucal_openCountryTimeZones_4_0" ucal_openCountryTimeZones
    :: CString -> Ptr UErrorCode -> IO (Ptr UEnumeration)
foreign import ccall unsafe "unicode/ucal.h ucal_getDefaultTimeZone_4_0" ucal_getDefaultTimeZone
    :: Ptr UChar -> Int32 -> Ptr UErrorCode -> IO Int32
foreign import ccall unsafe "unicode/ucal.h ucal_setDefaultTimeZone_4_0" ucal_setDefaultTimeZone
    :: Ptr UChar -> Ptr UErrorCode -> IO ()
foreign import ccall unsafe "unicode/ucal.h ucal_getCanonicalTimeZoneID_4_0" ucal_getCanonicalTimeZoneID
    :: Ptr UChar -> Int32 -> Ptr UChar -> Int32 -> Ptr UBool -> Ptr UErrorCode -> IO Int32
foreign import ccall unsafe "unicode/ucal.h ucal_equivalentTo_4_0" ucal_equivalentTo
    :: Ptr UCalendar -> Ptr UCalendar -> IO UBool
foreign import ccall unsafe "unicode/ucal.h ucal_getAttribute_4_0" ucal_getAttribute
    :: Ptr UCalendar -> Word32 -> IO Int32
foreign import ccall unsafe "unicode/ucal.h ucal_setAttribute_4_0" ucal_setAttribute
    :: Ptr UCalendar -> Word32 -> Int32 -> IO ()
foreign import ccall unsafe "unicode/ucal.h ucal_clear_4_0" ucal_clear
    :: Ptr UCalendar -> IO ()

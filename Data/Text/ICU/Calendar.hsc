{-# LANGUAGE CPP, DeriveDataTypeable, EmptyDataDecls, ForeignFunctionInterface,
    GeneralizedNewtypeDeriving, OverloadedStrings, StandaloneDeriving, TypeFamilies, MultiParamTypeClasses #-}
-- | This module is used for converting between a 'Date' and a set of
-- 'Int' fields such as 'year', 'month', 'day', 'hour', and so on. A
-- 'Date' represents a specific instant in time with millisecond
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
    -- * Types
      CalendarType(..)
    , Weekday(..)
    , Month(..)
    , AmPm(..)
    , DisplayName(..)
    , Attribute(..)
    , Limit(..),
     -- * Date
     Date,UDate,fromDate,getNow,
     -- * Calendar creation and manipulation
     Calendar,calendarType,calendarLocale,calendarZoneID,openCalendar,cloneCalendar,
     getMillis,setMillis,
     clearCalendar
     -- ** Fields
    , Field(..)
    , MutableField(..)
    , amPm
    , dayOfMonth
    , dayOfWeek
    , dayOfWeekInMonth
    , dayOfYear
    , dstOffset
    , era
    , extendedYear
    , hour
    , hourOfDay
    , isLeapMonth
    , julianDay
    , localDayOfWeek
    , millisecond
    , millisecondsInDay
    , minute
    , month
    , second
    , weekOfMonth
    , weekOfYear
    , year
    , yearWOY
    , zoneOffset
    , AmPmField
    , DayOfMonthField
    , DayOfWeekField
    , DayOfWeekInMonthField
    , DayOfYearField
    , DstOffsetField
    , EraField
    , ExtendedYearField
    , HourField
    , HourOfDayField
    , IsLeapMonthField
    , JulianDayField
    , LocalDayOfWeekField
    , MillisecondField
    , MillisecondsInDayField
    , MinuteField
    , MonthField
    , SecondField
    , WeekOfMonthField
    , WeekOfYearField
    , YearField
    , YearWOYField
    , ZoneOffsetField
     -- * Time zones
    , openTimeZones,timeZones,openCountryTimeZones,countryTimeZones,getDefaultTimeZone,
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
import Data.Text.ICU.Error.Codes (UErrorCode)
import Data.Text.ICU.Error.Internal (handleError)
import Data.Text.ICU.Internal (UChar)
import Data.Text.ICU.Locale.Internal (Locale(..))
import Data.Text.ICU.Enumeration (Enumeration,UEnumeration,enumerationFinalizer,enumerationTexts)
import Data.Typeable (Typeable)
import Data.Int (Int32)
import Data.Word (Word8)
import Foreign.C.String (CString,withCAString)
import Foreign.C.Types (CDouble, CInt)
import Foreign.ForeignPtr (ForeignPtr,newForeignPtr,withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr,FunPtr)
import Foreign.Storable (peek)
import System.IO.Unsafe (unsafePerformIO)

-- | Possible types of 'Calendar's.
data CalendarType = Traditional -- ^ For the locale.
                  | Gregorian   -- ^ Gregorian calendar.
                    deriving (Eq, Ord, Show, Enum, Bounded, Typeable, Data)

toICUDay :: Weekday -> Int32
toICUDay = (+1) . fromIntegral . fromEnum

fromICUDay :: Int32 -> Weekday
fromICUDay = toEnum . fromIntegral . subtract 1

-- | Days of the week.
data Weekday = Sunday
             | Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
               deriving (Eq, Ord, Show, Enum, Bounded, Typeable, Data)

-- | Months of the year.
data Month = January
           | February
           | April
           | May
           | June
           | July
           | August
           | September
           | October
           | November
           | December
           | Undecimber
             -- ^ The thirteenth month of the year. Although the
             -- Gregorian calendar does not use this value, lunar
             -- calendars do.
             deriving (Eq, Ord, Show, Enum, Bounded, Typeable, Data)

-- | Before/after noon.
data AmPm = AM
          | PM
            deriving (Eq, Ord, Show, Enum, Bounded, Typeable, Data)

-- | Possible formats for the display name of a 'Calendar'.
data DisplayName = Standard
                 | ShortStandard
                 | DST
                 | ShortDST
                   deriving (Eq, Ord, Show, Enum, Bounded, Typeable, Data)

-- Attributes of a 'Calendar'.
data Attribute = Lenient        -- ^ Lenient parsing.
               | FirstDayOfWeek -- ^ First day of week.
               | MinimalDaysInFirstWeek -- ^ Minimum number of days in first week.
                 deriving (Eq, Ord, Show, Enum, Bounded, Typeable, Data)

data Limit = Minimum
           | Maximum
           | GreatestMinimem
           | LeastMaximum
           | ActualMinimum
           | ActualMaximum
             deriving (Eq, Ord, Show, Enum, Bounded, Typeable, Data)

data UCalendar
data Calendar = Calendar {
      calendarType :: CalendarType,
      calendarLocale :: Locale,
      calendarZoneID :: Text,
      uCalendar :: ForeignPtr UCalendar
    }
                deriving (Typeable)

-- | A field of a 'Calendar'.
class Field a where
    -- | The type of the value stored in the given field.
    type FieldAt a
    -- | Get the value of a field in a 'Calendar'.
    getField :: Calendar -> a -> IO (FieldAt a)

-- | A field whose value can be directly modified.
class Field a => MutableField a where
    -- | Set the value of a field in a 'Calendar'.
    setField :: Calendar -> a -> FieldAt a -> IO ()
    -- | Add to a field.
    --
    -- If the amount added causes the value to exceed maximum or
    -- minimum values for that field, other fields are modified to
    -- preserve the magnitude of the change.
    addField :: Calendar -> a -> Int -> IO ()
    -- | Add to a field. This will /not/ modify more significant
    -- fields in the calendar.
    --
    -- If the amount added would cause the value to exceed maximum or
    -- minimum values for that field, the field is pinned to a
    -- permissible value.
    rollField :: Calendar -> a -> Int -> IO ()

-- | Era, e.g. AD or BC in the Gregorian (Julian) calendar.  This is a
-- calendar-specific value.
data EraField = Era deriving (Show, Typeable, Data)
era :: EraField
era = Era

-- | Year. This is a calendar-specific value.
data YearField = Year deriving (Show, Typeable, Data)
year :: YearField
year = Year

-- | Month. This field has type 'Month'.  This is a calendar-specific
-- value. The first month of the year is 'January'; the last depends
-- on the number of months in a year.
data MonthField = Month deriving (Show, Typeable, Data)
month :: MonthField
month = Month

-- | Week number within the current year.  The first week of the year,
-- as defined by 'firstDayOfWeek' and 'minimalDaysInFirstWeek'
-- attributes, has value 1. Subclasses define the value of
-- 'weekOfYear' for days before the first week of the year.
data WeekOfYearField = WeekOfYear deriving (Show, Typeable, Data)
weekOfYear :: WeekOfYearField
weekOfYear = WeekOfYear

-- | Week number within the current month.  The first week of the
-- month, as defined by 'firstDayOfWeek' and 'minimalDaysInFirstWeek'
-- attributes, has value 1.
data WeekOfMonthField = WeekOfMonth deriving (Show, Typeable, Data)
weekOfMonth :: WeekOfMonthField
weekOfMonth = WeekOfMonth

-- | Day of the month.  The first day of the month has value 1.
data DayOfMonthField = DayOfMonth deriving (Show, Typeable, Data)
dayOfMonth :: DayOfMonthField
dayOfMonth = DayOfMonth

-- | Day number within the current year. The first day of the year has
-- value 1.
data DayOfYearField = DayOfYear deriving (Show, Typeable, Data)
dayOfYear :: DayOfYearField
dayOfYear = DayOfYear

-- | Day of the week. This field has type 'Weekday'.
data DayOfWeekField = DayOfWeek deriving (Show, Typeable, Data)
dayOfWeek :: DayOfWeekField
dayOfWeek = DayOfWeek

-- | Ordinal number of the day of the week within the current
-- month. Together with the 'dayOfWeek' field, this uniquely specifies
-- a day within a month. Unlike 'weekOfMonth' and 'weekOfYear', this
-- field's value does not depend on getFirstDayOfWeek or
-- getMinimalDaysInFirstWeek. Values of 'dayOfMonth' 1 through 7
-- always correspond to 'dayOfWeekInMonth' 1; 8 through 15 correspond
-- to 'dayOfWeekInMonth' 2, and so on. 'dayOfWeekInMonth' 0 indicates
-- the week before 'dayOfWeekInMonth' 1. Negative values count back
-- from the end of the month, so the last Sunday of a month is
-- specified as 'dayOfWeek' = 'sunday', 'dayOfWeekInMonth'@ =
-- -1@. Because negative values count backward they will usually be
-- aligned differently within the month than positive values. For
-- example, if a month has 31 days, 'dayOfWeekInMonth' -1 will overlap
-- 'dayOfWeekInMonth' 5 and the end of 4.
data DayOfWeekInMonthField = DayOfWeekInMonth deriving (Show, Typeable, Data)
dayOfWeekInMonth :: DayOfWeekInMonthField
dayOfWeekInMonth = DayOfWeekInMonth

-- | Whether the hour is before or after noon. This field has type
-- 'AmPm'. For example, at 10:04:15.250 PM, the 'amPm' field is 'PM'.
data AmPmField = AmPm deriving (Show, Typeable, Data)
amPm :: AmPmField
amPm = AmPm

-- | Hour of the morning or afternoon, used for the 12-hour clock. For
-- example, at 10:04:15.250 PM, the 'hour' field is 10.
data HourField = Hour deriving (Show, Typeable, Data)
hour :: HourField
hour = Hour

-- | Hour of the day, used for the 24-hour clock. For example, at
-- 10:04:15.250 PM, the 'hourOfDay' field is 22.
data HourOfDayField = HourOfDay deriving (Show, Typeable, Data)
hourOfDay :: HourOfDayField
hourOfDay = HourOfDay

-- | Minute within the hour.  For example, at 10:04:15.250 PM the
-- 'minute' field is 4.
data MinuteField = Minute deriving (Show, Typeable, Data)
minute :: MinuteField
minute = Minute

-- | Second within the minute. For example, at 10:04:15.250 PM, the
-- 'second' field is 15.
data SecondField = Second deriving (Show, Typeable, Data)
second :: SecondField
second = Second

-- | Millisecond within the second.
data MillisecondField = Millisecond deriving (Show, Typeable, Data)
millisecond :: MillisecondField
millisecond = Millisecond

-- | Raw offset from GMT in milliseconds.
data ZoneOffsetField = ZoneOffset deriving (Show, Typeable, Data)
zoneOffset :: ZoneOffsetField
zoneOffset = ZoneOffset

-- | Daylight savings offset in milliseconds.
data DstOffsetField = DstOffset deriving (Show, Typeable, Data)
dstOffset :: DstOffsetField
dstOffset = DstOffset

-- | Extended year corresponding to the 'weekOfYear' field.  This may
-- be one greater or less than the value of 'extendedYear'.
data YearWOYField = YearWOY deriving (Show, Typeable, Data)
yearWOY :: YearWOYField
yearWOY = YearWOY

-- | Localized day of week.  This field has type 'Weekday'.
data LocalDayOfWeekField = LocalDayOfWeek deriving (Show, Typeable, Data)
localDayOfWeek :: LocalDayOfWeekField
localDayOfWeek = LocalDayOfWeek

-- | Year of this calendar system, encompassing all supra-year
-- fields. For example, in Gregorian/Julian calendars, positive
-- extended year values indicate years AD, 1 BC = 0 extended, 2 BC =
-- -1 extended, and so on.
data ExtendedYearField = ExtendedYear deriving (Show, Typeable, Data)
extendedYear :: ExtendedYearField
extendedYear = ExtendedYear

-- | Modified Julian day number. This is different from the
-- conventional Julian day number in two regards:

-- * It demarcates days at local zone midnight, rather than noon GMT.
--
-- * It is a local number; that is, it depends on the local time zone.
--
-- It can be thought of as a single number that encompasses all the
-- date-related fields.
data JulianDayField = JulianDay deriving (Show, Typeable, Data)
julianDay :: JulianDayField
julianDay = JulianDay

-- | Ranges from 0 to 23:59:59.999 (regardless of DST). This field
-- behaves exactly like a composite of all time-related fields, not
-- including the zone fields. As such, it also reflects
-- discontinuities of those fields on DST transition days. On a day of
-- DST onset, it will jump forward. On a day of DST cessation, it will
-- jump backward. This reflects the fact that it must be combined with
-- the 'dstOffset' field to obtain a unique local time value.
data MillisecondsInDayField = MillisecondsInDay deriving (Show, Typeable, Data)
millisecondsInDay :: MillisecondsInDayField
millisecondsInDay = MillisecondsInDay

-- | Whether or not the current month is a leap month.  This field has
-- type 'Bool'.
data IsLeapMonthField = IsLeapMonth deriving (Show, Typeable, Data)
isLeapMonth :: IsLeapMonthField
isLeapMonth = IsLeapMonth

instance Field EraField where
    type FieldAt EraField = Int
    getField cal _ = getCal cal (#const UCAL_ERA) fromIntegral

instance MutableField EraField where
    setField  cal _ = setCal  cal (#const UCAL_ERA) fromIntegral
    addField  cal _ = addCal  cal (#const UCAL_ERA)
    rollField cal _ = rollCal cal (#const UCAL_ERA)

instance Field YearField where
    type FieldAt YearField = Int
    getField cal _ = getCal cal (#const UCAL_YEAR) fromIntegral

instance MutableField YearField where
    setField  cal _ = setCal  cal (#const UCAL_YEAR) fromIntegral
    addField  cal _ = addCal  cal (#const UCAL_YEAR)
    rollField cal _ = rollCal cal (#const UCAL_YEAR)

instance Field MonthField where
    type FieldAt MonthField = Int
    getField cal _ = getCal cal (#const UCAL_MONTH) (toEnum . fromIntegral)

instance MutableField MonthField where
    setField  cal _ = setCal  cal (#const UCAL_MONTH) (fromIntegral . fromEnum)
    addField  cal _ = addCal  cal (#const UCAL_MONTH)
    rollField cal _ = rollCal cal (#const UCAL_MONTH)

instance Field WeekOfYearField where
    type FieldAt WeekOfYearField = Int
    getField cal _ = getCal cal (#const UCAL_WEEK_OF_YEAR) fromIntegral

instance MutableField WeekOfYearField where
    setField  cal _ = setCal  cal (#const UCAL_WEEK_OF_YEAR) fromIntegral
    addField  cal _ = addCal  cal (#const UCAL_WEEK_OF_YEAR)
    rollField cal _ = rollCal cal (#const UCAL_WEEK_OF_YEAR)

instance Field WeekOfMonthField where
    type FieldAt WeekOfMonthField = Int
    getField cal _ = getCal cal (#const UCAL_WEEK_OF_MONTH) fromIntegral

instance MutableField WeekOfMonthField where
    setField  cal _ = setCal  cal (#const UCAL_WEEK_OF_MONTH) fromIntegral
    addField  cal _ = addCal  cal (#const UCAL_WEEK_OF_MONTH)
    rollField cal _ = rollCal cal (#const UCAL_WEEK_OF_MONTH)

instance Field DayOfMonthField where
    type FieldAt DayOfMonthField = Int
    getField cal _ = getCal cal (#const UCAL_DAY_OF_MONTH) fromIntegral

instance MutableField DayOfMonthField where
    setField  cal _ = setCal  cal (#const UCAL_DAY_OF_MONTH) fromIntegral
    addField  cal _ = addCal  cal (#const UCAL_DAY_OF_MONTH)
    rollField cal _ = rollCal cal (#const UCAL_DAY_OF_MONTH)

instance Field DayOfYearField where
    type FieldAt DayOfYearField = Int
    getField cal _ = getCal cal (#const UCAL_DAY_OF_YEAR) fromIntegral

instance MutableField DayOfYearField where
    setField  cal _ = setCal  cal (#const UCAL_DAY_OF_YEAR) fromIntegral
    addField  cal _ = addCal  cal (#const UCAL_DAY_OF_YEAR)
    rollField cal _ = rollCal cal (#const UCAL_DAY_OF_YEAR)

instance Field DayOfWeekField where
    type FieldAt DayOfWeekField = Weekday
    getField cal _ = getCal cal (#const UCAL_DAY_OF_WEEK) fromICUDay

instance MutableField DayOfWeekField where
    setField  cal _ = setCal  cal (#const UCAL_DAY_OF_WEEK) toICUDay
    addField  cal _ = addCal  cal (#const UCAL_DAY_OF_WEEK)
    rollField cal _ = rollCal cal (#const UCAL_DAY_OF_WEEK)

instance Field DayOfWeekInMonthField where
    type FieldAt DayOfWeekInMonthField = Int
    getField cal _ = getCal cal (#const UCAL_DAY_OF_WEEK_IN_MONTH) fromIntegral

instance MutableField DayOfWeekInMonthField where
    setField  cal _ = setCal  cal (#const UCAL_DAY_OF_WEEK_IN_MONTH) fromIntegral
    addField  cal _ = addCal  cal (#const UCAL_DAY_OF_WEEK_IN_MONTH)
    rollField cal _ = rollCal cal (#const UCAL_DAY_OF_WEEK_IN_MONTH)

instance Field AmPmField where
    type FieldAt AmPmField = AmPm
    getField cal _ = getCal cal (#const UCAL_AM_PM) (toEnum . fromIntegral)

instance MutableField AmPmField where
    setField  cal _ = setCal  cal (#const UCAL_AM_PM) (fromIntegral . fromEnum)
    addField  cal _ = addCal  cal (#const UCAL_AM_PM)
    rollField cal _ = rollCal cal (#const UCAL_AM_PM)

instance Field HourField where
    type FieldAt HourField = Int
    getField cal _ = getCal cal (#const UCAL_HOUR) fromIntegral

instance MutableField HourField where
    setField  cal _ = setCal  cal (#const UCAL_HOUR) fromIntegral
    addField  cal _ = addCal  cal (#const UCAL_HOUR)
    rollField cal _ = rollCal cal (#const UCAL_HOUR)

instance Field HourOfDayField where
    type FieldAt HourOfDayField = Int
    getField cal _ = getCal cal (#const UCAL_HOUR_OF_DAY) fromIntegral

instance MutableField HourOfDayField where
    setField  cal _ = setCal  cal (#const UCAL_HOUR_OF_DAY) fromIntegral
    addField  cal _ = addCal  cal (#const UCAL_HOUR_OF_DAY)
    rollField cal _ = rollCal cal (#const UCAL_HOUR_OF_DAY)

instance Field MinuteField where
    type FieldAt MinuteField = Int
    getField cal _ = getCal cal (#const UCAL_MINUTE) fromIntegral

instance MutableField MinuteField where
    setField  cal _ = setCal  cal (#const UCAL_MINUTE) fromIntegral
    addField  cal _ = addCal  cal (#const UCAL_MINUTE)
    rollField cal _ = rollCal cal (#const UCAL_MINUTE)

instance Field SecondField where
    type FieldAt SecondField = Int
    getField cal _ = getCal cal (#const UCAL_SECOND) fromIntegral

instance MutableField SecondField where
    setField  cal _ = setCal  cal (#const UCAL_SECOND) fromIntegral
    addField  cal _ = addCal  cal (#const UCAL_SECOND)
    rollField cal _ = rollCal cal (#const UCAL_SECOND)

instance Field MillisecondField where
    type FieldAt MillisecondField = Int
    getField cal _ = getCal cal (#const UCAL_MILLISECOND) fromIntegral

instance MutableField MillisecondField where
    setField  cal _ = setCal  cal (#const UCAL_MILLISECOND) fromIntegral
    addField  cal _ = addCal  cal (#const UCAL_MILLISECOND)
    rollField cal _ = rollCal cal (#const UCAL_MILLISECOND)

instance Field ZoneOffsetField where
    type FieldAt ZoneOffsetField = Int
    getField cal _ = getCal cal (#const UCAL_ZONE_OFFSET) fromIntegral

instance MutableField ZoneOffsetField where
    setField  cal _ = setCal  cal (#const UCAL_ZONE_OFFSET) fromIntegral
    addField  cal _ = addCal  cal (#const UCAL_ZONE_OFFSET)
    rollField cal _ = rollCal cal (#const UCAL_ZONE_OFFSET)

instance Field DstOffsetField where
    type FieldAt DstOffsetField = Int
    getField cal _ = getCal cal (#const UCAL_DST_OFFSET) fromIntegral

instance MutableField DstOffsetField where
    setField  cal _ = setCal  cal (#const UCAL_DST_OFFSET) fromIntegral
    addField  cal _ = addCal  cal (#const UCAL_DST_OFFSET)
    rollField cal _ = rollCal cal (#const UCAL_DST_OFFSET)

instance Field YearWOYField where
    type FieldAt YearWOYField = Int
    getField cal _ = getCal cal (#const UCAL_YEAR_WOY) fromIntegral

instance Field LocalDayOfWeekField where
    type FieldAt LocalDayOfWeekField = Weekday
    getField cal _ = getCal cal (#const UCAL_DOW_LOCAL) fromICUDay

instance Field ExtendedYearField where
    type FieldAt ExtendedYearField = Int
    getField cal _ = getCal cal (#const UCAL_EXTENDED_YEAR) fromIntegral

instance Field JulianDayField where
    type FieldAt JulianDayField = Int
    getField cal _ = getCal cal (#const UCAL_JULIAN_DAY) fromIntegral

instance Field MillisecondsInDayField where
    type FieldAt MillisecondsInDayField = Int
    getField cal _ = getCal cal (#const UCAL_MILLISECONDS_IN_DAY) fromIntegral

instance Field IsLeapMonthField where
    type FieldAt IsLeapMonthField = Bool
    getField cal _ = getCal cal (#const UCAL_IS_LEAP_MONTH) (/=0)

type UDate = CDouble
newtype Date = DDate {fromDate :: Double} deriving (Eq,Ord,Show,Typeable,Data)

-- | Get the current date and time. The value returned is represented as
-- milliseconds from the epoch.
getNow :: IO Date
getNow = DDate . fromRational . toRational <$> ucal_getNow

-- | Open a 'Calendar'. A 'Calendar' may be used to convert a
-- millisecond value to a year, month, and day.
openCalendar :: Text      -- ^ The desired time zone ID. If empty, use the default time zone.
             -> Locale          -- ^ The desired locale.
             -> CalendarType    -- ^ The type of calendar to open.
             -> IO Calendar
openCalendar z loc t = do
  useAsPtr z $ \zoneID' l -> do
    withCAString (localeName loc) $ \locale' -> do
      cal <- handleError $ ucal_open zoneID' (fromIntegral l) locale' (fromIntegral (fromEnum t))
      Calendar t loc z <$> newForeignPtr ucal_close cal

-- | Open a copy of a 'Calendar'. This function performs a deep copy.
cloneCalendar :: Calendar -> IO Calendar
cloneCalendar (Calendar t loc mz ucal) = do
  withForeignPtr ucal $ \ucal' -> do
    newCal <- handleError $ ucal_clone ucal'
    Calendar t loc mz <$> newForeignPtr ucal_close newCal

-- | Get a 'Calendar''s current time in milliseconds as a 'Date'. The
-- time is represented as milliseconds from the epoch.
getMillis :: Calendar -> IO Date
getMillis cal =
  withForeignPtr (uCalendar cal) $
    fmap (DDate . fromRational . toRational) . handleError . ucal_getMillis

-- | Set a 'Calendar''s current time in milliseconds from a 'Date'. The time
-- is represented as milliseconds from the epoch.
setMillis :: Calendar -> Date -> IO ()
setMillis cal d = do
  withForeignPtr (uCalendar cal) $ \cal' ->
    handleError $ ucal_setMillis cal' (fromRational (toRational (fromDate d)))

-- | Get the value of a field in a 'Calendar'. All fields are
-- represented as 32-bit integers.
getCal :: Calendar -> CInt -> (Int32 -> b) -> IO b
getCal cal field f =
  withForeignPtr (uCalendar cal) $ \cal' ->
    fmap f . handleError $ ucal_get cal' field

-- | Set the value of a field in a 'Calendar'.
setCal :: Calendar -> CInt -> (a -> Int32) -> a -> IO ()
setCal cal field f x =
  withForeignPtr (uCalendar cal) $ \cal' ->
    ucal_set cal' field (f x)

-- | Add to the value of a field in a 'Calendar'.
addCal :: Calendar -> CInt -> Int -> IO ()
addCal cal field x =
  withForeignPtr (uCalendar cal) $ \cal' ->
    handleError $ ucal_add cal' field (fromIntegral x)

-- | Add to the value of a field in a 'Calendar'.
rollCal :: Calendar -> CInt -> Int -> IO ()
rollCal cal field x =
  withForeignPtr (uCalendar cal) $ \cal' ->
    handleError $ ucal_roll cal' field (fromIntegral x)

-- | Clear all fields in a 'Calendar'.
clearCalendar :: Calendar -> IO ()
clearCalendar cal = withForeignPtr (uCalendar cal) ucal_clear

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
    :: Ptr UChar -> Int32 -> CString -> CInt -> Ptr UErrorCode -> IO (Ptr UCalendar)
foreign import ccall "unicode/ucal.h &ucal_close_4_0" ucal_close
    :: FunPtr (Ptr UCalendar -> IO ())
foreign import ccall unsafe "unicode/ucal.h ucal_getMillis_4_0" ucal_getMillis
    :: Ptr UCalendar -> Ptr UErrorCode -> IO UDate
foreign import ccall unsafe "unicode/ucal.h ucal_setMillis_4_0" ucal_setMillis
    :: Ptr UCalendar -> UDate -> Ptr UErrorCode -> IO ()
foreign import ccall unsafe "unicode/ucal.h ucal_get_4_0" ucal_get
    :: Ptr UCalendar -> CInt -> Ptr UErrorCode -> IO Int32
foreign import ccall unsafe "unicode/ucal.h ucal_set_4_0" ucal_set
    :: Ptr UCalendar -> CInt -> Int32 -> IO ()
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
--foreign import ccall unsafe "unicode/ucal.h ucal_getAttribute_4_0" ucal_getAttribute
--  :: Ptr UCalendar -> Word32 -> IO Int32
--foreign import ccall unsafe "unicode/ucal.h ucal_setAttribute_4_0" ucal_setAttribute
--  :: Ptr UCalendar -> Word32 -> Int32 -> IO ()
foreign import ccall unsafe "unicode/ucal.h ucal_clear_4_0" ucal_clear
    :: Ptr UCalendar -> IO ()
foreign import ccall unsafe "unicode/ucal.h ucal_add_4_0" ucal_add
    :: Ptr UCalendar -> CInt -> Int32 -> Ptr UErrorCode -> IO ()
foreign import ccall unsafe "unicode/ucal.h ucal_roll_4_0" ucal_roll
    :: Ptr UCalendar -> CInt -> Int32 -> Ptr UErrorCode -> IO ()

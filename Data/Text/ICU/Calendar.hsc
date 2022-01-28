{-# LANGUAGE ImportQualifiedPost, RankNTypes, BangPatterns, ForeignFunctionInterface, RecordWildCards #-}
-- |
-- Module      : Data.Text.ICU.Calendar
-- Copyright   : (c) 2021 Torsten Kemps-Benedix
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Calendar functions implemented as bindings to
-- the International Components for Unicode (ICU) libraries.

module Data.Text.ICU.Calendar
    (
      -- * Data
      Calendar(..), CalendarType(..), SystemTimeZoneType(..), CalendarField(..), UCalendar,
      -- * High-level interface
      -- ** Operations on calendars
      roll, add, set1, set, get,
      -- ** Calendar field getters
      era, year, month,
      dayOfMonth, dayOfYear, dayOfWeek, dayOfWeekInMonth, amPm,
      hour, hourOfDay, minute, second, millisecond, zoneOffset, dstOffset, yearWoY, doWLocal, extendedYear, julianDay,
      millisecondsInDay, isLeapMonth, day, utcTime,
      -- ** Calendar field setters
      setEra, setYear, setMonth,
      setDayOfMonth, setDayOfYear, setDayOfWeek, setDayOfWeekInMonth, setAmPm,
      setHour, setHourOfDay, setMinute, setSecond, setMillisecond, setZoneOffset, setDstOffset, setYearWoY, setDoWLocal, setExtendedYear, setJulianDay,
      setMillisecondsInDay, setDay,
      -- ** Lenses
      _era, _year, _month,
      _dayOfMonth, _dayOfYear, _dayOfWeek, _dayOfWeekInMonth, _amPm,
      _hour, _hourOfDay, _minute, _second, _millisecond, _zoneOffset, _dstOffset, _day,
      -- ** Time zone functions
      timeZoneIDs, timeZones,
      -- * Low-level interface
      calendar, openTimeZoneIDEnumeration, openTimeZones, getField, setField, setDate, setDateTime, rollField, addField, setTimeZone,
    ) where

#include <unicode/ucal.h>

import Control.Monad (forM_)
import Data.Int (Int32)
import Data.Text (Text, pack)
import Data.Text.Foreign (withCStringLen)
import Data.Text.ICU.Enumerator
import Data.Text.ICU.Error.Internal (UErrorCode, handleError)
import Data.Text.ICU.Internal (LocaleName(..), UChar, withLocaleName, newICUPtr, useAsUCharPtr)
import Data.Time.Calendar qualified as Cal
import Data.Time.Clock qualified as Clock
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..))
import Foreign.ForeignPtr (withForeignPtr, ForeignPtr)
import Foreign.Ptr (FunPtr, Ptr)
import Prelude hiding (last)
import System.IO.Unsafe (unsafePerformIO)

type UCalendar = CInt

-- A 'Calendar' is an absttract data type that contains a foreign pointer to the ICU internal data structure.
data Calendar = Calendar {calendarForeignPtr :: ForeignPtr UCalendar}

-- | All the fields that comprise a 'Calendar'.
data CalendarField =
  Era -- ^ Field indicating the era, e.g., AD or BC in the Gregorian (Julian) calendar. This is a calendar-specific value.
  | Year -- ^ Field indicating the year. This is a calendar-specific value.
  | Month -- ^ Field indicating the month. This is a calendar-specific value. The first month of the year is JANUARY; the last depends on the number of months in a year. Note: Calendar month is 0-based.
  | WeekOfYear -- ^ Field indicating the week number within the current year. The first week of the year, as defined by UCAL_FIRST_DAY_OF_WEEK and UCAL_MINIMAL_DAYS_IN_FIRST_WEEK attributes, has value 1. Subclasses define the value of UCAL_WEEK_OF_YEAR for days before the first week of the year.
  | WeekOfMonth -- ^ Field indicating the week number within the current month. The first week of the month, as defined by UCAL_FIRST_DAY_OF_WEEK and UCAL_MINIMAL_DAYS_IN_FIRST_WEEK attributes, has value 1. Subclasses define the value of WEEK_OF_MONTH for days before the first week of the month.
  | DayOfMonth -- ^ Field indicating the day of the month. This is a synonym for DAY_OF_MONTH. The first day of the month has value 1.
  | DayOfYear -- ^ Field indicating the day number within the current year. The first day of the year has value 1.
  | DayOfWeek -- ^ Field indicating the day of the week. This field takes values SUNDAY, MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY, and SATURDAY. Note: Calendar day-of-week is 1-based. Clients who create locale resources for the field of first-day-of-week should be aware of this. For instance, in US locale, first-day-of-week is set to 1, i.e., UCAL_SUNDAY.
  | DayOfWeekInMonth -- ^ Field indicating the ordinal number of the day of the week within the current month. Together with the DAY_OF_WEEK field, this uniquely specifies a day within a month. Unlike WEEK_OF_MONTH and WEEK_OF_YEAR, this field's value does not depend on getFirstDayOfWeek() or getMinimalDaysInFirstWeek(). DAY_OF_MONTH 1 through 7 always correspond to DAY_OF_WEEK_IN_MONTH 1; 8 through 15 correspond to DAY_OF_WEEK_IN_MONTH 2, and so on. DAY_OF_WEEK_IN_MONTH 0 indicates the week before DAY_OF_WEEK_IN_MONTH 1. Negative values count back from the end of the month, so the last Sunday of a month is specified as DAY_OF_WEEK = SUNDAY, DAY_OF_WEEK_IN_MONTH = -1. Because negative values count backward they will usually be aligned differently within the month than positive values. For example, if a month has 31 days, DAY_OF_WEEK_IN_MONTH -1 will overlap DAY_OF_WEEK_IN_MONTH 5 and the end of 4.
  | AmPm -- ^ Field indicating whether the HOUR is before or after noon. E.g., at 10:04:15.250 PM the AM_PM is PM.
  | Hour -- ^ Field indicating the hour of the morning or afternoon. HOUR is used for the 12-hour clock. E.g., at 10:04:15.250 PM the HOUR is 10.
  | HourOfDay -- ^ Field indicating the hour of the day. HOUR_OF_DAY is used for the 24-hour clock. E.g., at 10:04:15.250 PM the HOUR_OF_DAY is 22.
  | Minute -- ^ Field indicating the minute within the hour. E.g., at 10:04:15.250 PM the UCAL_MINUTE is 4.
  | Second -- ^ Field indicating the second within the minute. E.g., at 10:04:15.250 PM the UCAL_SECOND is 15.
  | Millisecond -- ^ Field indicating the millisecond within the second. E.g., at 10:04:15.250 PM the UCAL_MILLISECOND is 250.
  | ZoneOffset -- ^ Field indicating the raw offset from GMT in milliseconds.
  | DstOffset -- ^ Field indicating the daylight savings offset in milliseconds.
  | YearWoY -- ^ Field indicating the extended year corresponding to the UCAL_WEEK_OF_YEAR field. This may be one greater or less than the value of UCAL_EXTENDED_YEAR.
  | DoWLocal -- ^ Field indicating the localized day of week. This will be a value from 1 to 7 inclusive, with 1 being the localized first day of the week.
  | ExtendedYear -- ^ Year of this calendar system, encompassing all supra-year fields. For example, in Gregorian/Julian calendars, positive Extended Year values indicate years AD, 1 BC = 0 extended, 2 BC = -1 extended, and so on.
  | JulianDay -- ^ Field indicating the modified Julian day number. This is different from the conventional Julian day number in two regards. First, it demarcates days at local zone midnight, rather than noon GMT. Second, it is a local number; that is, it depends on the local time zone. It can be thought of as a single number that encompasses all the date-related fields.
  | MillisecondsInDay -- ^ Ranges from 0 to 23:59:59.999 (regardless of DST). This field behaves exactly like a composite of all time-related fields, not including the zone fields. As such, it also reflects discontinuities of those fields on DST transition days. On a day of DST onset, it will jump forward. On a day of DST cessation, it will jump backward. This reflects the fact that it must be combined with the DST_OFFSET field to obtain a unique local time value.
  | IsLeapMonth -- ^ Whether or not the current month is a leap month (0 or 1). See the Chinese calendar for an example of this.
  deriving (Show, Read, Eq)

type UCalendarDateFields = CInt

toUCalendarDateFields :: CalendarField -> UCalendarDateFields
toUCalendarDateFields Era = #const UCAL_ERA
toUCalendarDateFields Year = #const UCAL_YEAR
toUCalendarDateFields Month = #const UCAL_MONTH
toUCalendarDateFields WeekOfYear = #const UCAL_WEEK_OF_YEAR
toUCalendarDateFields WeekOfMonth = #const UCAL_WEEK_OF_MONTH
toUCalendarDateFields DayOfMonth = #const UCAL_DAY_OF_MONTH
toUCalendarDateFields DayOfYear = #const UCAL_DAY_OF_YEAR
toUCalendarDateFields DayOfWeek = #const UCAL_DAY_OF_WEEK
toUCalendarDateFields DayOfWeekInMonth = #const UCAL_DAY_OF_WEEK_IN_MONTH
toUCalendarDateFields AmPm = #const UCAL_AM_PM
toUCalendarDateFields Hour = #const UCAL_HOUR
toUCalendarDateFields HourOfDay = #const UCAL_HOUR_OF_DAY
toUCalendarDateFields Minute = #const UCAL_MINUTE
toUCalendarDateFields Second = #const UCAL_SECOND
toUCalendarDateFields Millisecond = #const UCAL_MILLISECOND
toUCalendarDateFields ZoneOffset = #const UCAL_ZONE_OFFSET
toUCalendarDateFields DstOffset = #const UCAL_DST_OFFSET
toUCalendarDateFields YearWoY = #const UCAL_YEAR_WOY
toUCalendarDateFields DoWLocal = #const UCAL_DOW_LOCAL
toUCalendarDateFields ExtendedYear = #const UCAL_EXTENDED_YEAR
toUCalendarDateFields JulianDay = #const UCAL_JULIAN_DAY
toUCalendarDateFields MillisecondsInDay = #const UCAL_MILLISECONDS_IN_DAY
toUCalendarDateFields IsLeapMonth = #const UCAL_IS_LEAP_MONTH

data CalendarType = TraditionalCalendarType | DefaultCalendarType | GregorianCalendarType
  deriving (Show, Read, Eq)

type UCalendarType = CInt

toUCalendarType :: CalendarType -> UCalendarType
toUCalendarType TraditionalCalendarType = #const UCAL_TRADITIONAL
toUCalendarType DefaultCalendarType = #const UCAL_TRADITIONAL
toUCalendarType GregorianCalendarType = #const UCAL_GREGORIAN

data SystemTimeZoneType = AnyTimeZone | CanonicalTimeZone | CanonicalLocationTimeZone
  deriving (Show, Read, Eq)

toUSystemTimeZoneType :: SystemTimeZoneType -> USystemTimeZoneType
toUSystemTimeZoneType AnyTimeZone = #const UCAL_ZONE_TYPE_ANY
toUSystemTimeZoneType CanonicalTimeZone = #const UCAL_ZONE_TYPE_CANONICAL
toUSystemTimeZoneType CanonicalLocationTimeZone = #const UCAL_ZONE_TYPE_CANONICAL_LOCATION

type USystemTimeZoneType = CInt

-- | Open a Calendar.
--
-- A Calendar may be used to convert a millisecond value to a year,
-- month, and day.
--
-- Note: When unknown TimeZone ID is specified or if the TimeZone ID
-- specified is "Etc/Unknown", the Calendar returned by the function
-- is initialized with GMT zone with TimeZone ID UCAL_UNKNOWN_ZONE_ID
-- ("Etc/Unknown") without any errors/warnings. If you want to check
-- if a TimeZone ID is valid prior to this function, use
-- ucal_getCanonicalTimeZoneID.
--
-- >>> import qualified Data.Text as T
-- >>> c <- calendar (T.pack "CET") (Locale "de_DE") TraditionalCalendarType
-- >>> show c
-- 2021-10-12 17:37:43
calendar :: Text -> LocaleName -> CalendarType -> IO Calendar
calendar zoneId loc typ =
  withLocaleName loc $ \locale ->
    useAsUCharPtr zoneId $ \zoneIdPtr zoneIdLen ->
      newICUPtr Calendar ucal_close $
        handleError $ ucal_open zoneIdPtr (fromIntegral zoneIdLen) locale (toUCalendarType typ)

setTimeZone :: Calendar -> Text -> IO ()
setTimeZone cal zoneId =
  withForeignPtr (calendarForeignPtr cal) $ \calPtr -> do
    withCStringLen zoneId $ \(zoneIdPtr, zoneIdLen) -> do
      handleError $ ucal_setTimeZone calPtr zoneIdPtr (fromIntegral zoneIdLen)

clone :: Calendar -> IO Calendar
clone cal =
  withForeignPtr (calendarForeignPtr cal) $ \calPtr -> do
    newICUPtr Calendar ucal_close $ handleError $ ucal_clone calPtr

-- | List of all time zones.
timeZones :: IO [Text]
timeZones = do
  tzEnum <- openTimeZones
  tzs <- toList tzEnum
  pure tzs

timeZoneIDs :: SystemTimeZoneType -> IO [Text]
timeZoneIDs typ = do
  tzEnum <- openTimeZoneIDEnumeration typ
  tzs <- toList tzEnum
  pure tzs

openTimeZoneIDEnumeration :: SystemTimeZoneType -> IO Enumerator
openTimeZoneIDEnumeration typ = createEnumerator $
  handleError $ ucal_openTimeZoneIDEnumeration (toUSystemTimeZoneType typ)

-- | Create an enumeration over all time zones.
openTimeZones :: IO Enumerator
openTimeZones = createEnumerator $ handleError $ ucal_openTimeZones

-- | Get the value of a specific calendar field.
--
-- >>> import qualified Data.Text as T
-- >>> c <- calendar (T.pack "CET") (Locale "de_DE") TraditionalCalendarType
-- >>> getField c Year
-- 2021
getField :: Calendar -> CalendarField -> IO Int
getField cal fld = withForeignPtr (calendarForeignPtr cal) $ \calPtr -> do
  n <- handleError $ ucal_get calPtr (toUCalendarDateFields fld)
  pure (fromIntegral n)

setField :: Calendar -> CalendarField -> Int -> IO ()
setField cal fld n = withForeignPtr (calendarForeignPtr cal) $ \calPtr ->
  ucal_set calPtr (toUCalendarDateFields fld) (fromIntegral n)

setDate :: Calendar -> Int -> Int -> Int -> IO ()
setDate cal y m d = withForeignPtr (calendarForeignPtr cal) $ \calPtr ->
  handleError $ ucal_setDate calPtr (fromIntegral y) (fromIntegral m) (fromIntegral d)

setDateTime :: Calendar -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
setDateTime cal y m d hr mn sec = withForeignPtr (calendarForeignPtr cal) $ \calPtr ->
  handleError $ ucal_setDateTime calPtr (fromIntegral y) (fromIntegral m) (fromIntegral d) (fromIntegral hr) (fromIntegral mn) (fromIntegral sec)

-- | Add a specified signed amount to a particular field in a Calendar.
--
-- This will not modify more significant fields in the calendar. Rolling by a
-- positive value always means moving forward in time (unless the limit of the
-- field is reached, in which case it may pin or wrap), so for Gregorian
-- calendar, starting with 100 BC and rolling the year by +1 results in 99 BC.
-- When eras have a definite beginning and end (as in the Chinese calendar, or
-- as in most eras in the Japanese calendar) then rolling the year past either
-- limit of the era will cause the year to wrap around. When eras only have a
-- limit at one end, then attempting to roll the year past that limit will
--result in pinning the year at that limit. Note that for most calendars in
-- which era 0 years move forward in time (such as Buddhist, Hebrew, or Islamic),
-- it is possible for add or roll to result in negative years for era 0 (that
-- is the only way to represent years before the calendar epoch).
rollField :: Calendar -> CalendarField -> Int -> IO ()
rollField cal fld n = withForeignPtr (calendarForeignPtr cal) $ \calPtr ->
  handleError $ ucal_roll calPtr (toUCalendarDateFields fld) (fromIntegral n)

-- | Add a specified signed amount to a particular field in a Calendar.
--
-- See 'rollField' for further details.
--
-- >>> import qualified Data.Text as T
-- >>> c1 <- calendar (T.pack "CET") (Locale "de_DE") TraditionalCalendarType
-- >>> show c1
-- 2021-10-12 17:53:26
-- >>> let c2 = roll c1 [(Hour, 2)]
-- >>> show c2
-- 2021-10-12 19:53:26
-- >>> let c3 = roll c1 [(Hour, 12)]
-- >>> show c3
-- 2021-10-12 17:53:26
-- >>> let c4 = add c1 [(Hour, 12)]
-- >>> show c4
-- 2021-10-13 5:53:26
roll ::
  Calendar
  -> [(CalendarField, Int)] -- ^ The field and the signed amount to add to this field. If the amount causes the value to exceed to maximum or minimum values for that field, the field is pinned to a permissible value.
  -> Calendar
roll cal lst = unsafePerformIO $ do
  cal' <- clone cal
  forM_ lst (\(fld,n) -> rollField cal' fld n)
  pure cal'

-- | Add a specified signed amount to a particular field in a Calendar.
--
-- This can modify more significant fields in the calendar. Adding a positive
-- value always means moving forward in time, so for the Gregorian calendar,
-- starting with 100 BC and adding +1 to year results in 99 BC (even though
-- this actually reduces the numeric value of the field itself).
addField :: Calendar -> CalendarField -> Int -> IO ()
addField cal fld n = withForeignPtr (calendarForeignPtr cal) $ \calPtr ->
  handleError $ ucal_add calPtr (toUCalendarDateFields fld) (fromIntegral n)

-- | Add a specified signed amount to a particular field in a Calendar.
--
-- See 'addField' for further details and see 'rollField' for examples and differences
-- compared to rolling.
add ::
  Calendar -- ^ The 'Calendar' to which to add.
  -> [(CalendarField, Int)] -- ^ Field type and the signed amount to add to field. If the amount causes the value to exceed to maximum or minimum values for that field, other fields are modified to preserve the magnitude of the change.
  -> Calendar
add cal lst = unsafePerformIO $ do
  cal' <- clone cal
  forM_ lst (\(fld,n) -> addField cal' fld n)
  pure cal'

-- | Set the value of one field of a calendar to a certain value. All fields are
-- represented as 32-bit integers.
set1 :: Calendar -> CalendarField -> Int -> Calendar
set1 cal fld n = unsafePerformIO $ do
  cal' <- clone cal
  setField cal' fld n
  pure cal'

-- | Set the value of a list of fields of a calendar to certain values. All fields are
-- represented as 32-bit integers.
set :: Calendar -> [(CalendarField, Int)] -> Calendar
set cal lst = unsafePerformIO $ do
  cal' <- clone cal
  forM_ lst (\(fld,n) -> setField cal' fld n)
  pure cal'

-- | Convert the day part of the calendar to a 'Day'.
--
-- >>> import qualified Data.Text as T
-- >>> c1 <- calendar (T.pack "CET") (Locale "de_DE") TraditionalCalendarType
-- >>> show c1
-- 2021-10-12 18:00:50
-- >>> day c1
-- 2021-10-12
day :: Calendar -> Cal.Day
day cal = unsafePerformIO $ do
  y <- getField cal Year
  m <- getField cal Month
  d <- getField cal DayOfMonth
  pure (Cal.fromGregorian (fromIntegral y) (m+1) d)

-- | Set the day part of the calendar from a 'Day'.
setDay :: Calendar -> Cal.Day -> Calendar
setDay cal aDay = unsafePerformIO $ do
  cal' <- clone cal
  let (y,m,d) = Cal.toGregorian aDay
  setDate cal' (fromIntegral y) (m-1) d
  pure cal'

-- | Convert the day and time part of the calendar to a 'UTCTime'.
--
-- >>> import qualified Data.Text as T
-- >>> c1 <- calendar (T.pack "CET") (Locale "de_DE") TraditionalCalendarType
-- >>> show c1
-- 2021-10-12 18:00:50
-- >>> utcTime c1
-- 2021-10-12 16:00:50.544999999998 UTC
utcTime :: Calendar -> Clock.UTCTime
utcTime cal = unsafePerformIO $ do
  cal' <- clone cal
  setTimeZone cal' (pack "UTC")
  y <- getField cal' Year
  m <- getField cal' Month
  d <- getField cal' DayOfMonth
  let day' = Cal.fromGregorian (fromIntegral y) (m+1) d
  ms <- getField cal' MillisecondsInDay
  let dt = realToFrac ((fromIntegral ms :: Double) / 1000)
  pure $ Clock.UTCTime day' dt

-- | Get the value of a specific field in the calendar.
get :: Calendar -> CalendarField -> Int
get cal fld = unsafePerformIO $ getField cal fld

-- | Return the era of the calendar. The values are calendar specific and are usually 0 (some ancient era) and 1 (current era).
--
-- >>> import qualified Data.Text as T
-- >>> c1 <- calendar (T.pack "CET") (Locale "de_DE") TraditionalCalendarType
-- >>> era c1
-- 1
era :: Calendar -> Int
era cal = unsafePerformIO $ getField cal Era

setEra :: Calendar -> Int -> Calendar
setEra cal = set1 cal Era

-- | Return the year of the calendar. The values are calendar specific.
--
-- >>> import qualified Data.Text as T
-- >>> c1 <- calendar (T.pack "CET") (Locale "de_DE") TraditionalCalendarType
-- >>> year c1
-- 2021
year :: Calendar -> Int
year cal = unsafePerformIO $ getField cal Year

setYear :: Calendar -> Int -> Calendar
setYear cal = set1 cal Year

-- | Return the month of the calendar. The values are calendar specific and 0-based.
--
-- >>> import qualified Data.Text as T
-- >>> c1 <- calendar (T.pack "CET") (Locale "de_DE") TraditionalCalendarType
-- >>> month c1
-- 9
-- >>> day c1
-- 2021-10-12
month :: Calendar -> Int
month cal = unsafePerformIO $ getField cal Month

setMonth :: Calendar -> Int -> Calendar
setMonth cal = set1 cal Month

dayOfMonth :: Calendar -> Int
dayOfMonth cal = unsafePerformIO $ getField cal DayOfMonth

setDayOfMonth :: Calendar -> Int -> Calendar
setDayOfMonth cal = set1 cal DayOfMonth

dayOfYear :: Calendar -> Int
dayOfYear cal = unsafePerformIO $ getField cal DayOfYear

setDayOfYear :: Calendar -> Int -> Calendar
setDayOfYear cal = set1 cal DayOfYear

dayOfWeek :: Calendar -> Int
dayOfWeek cal = unsafePerformIO $ getField cal DayOfWeek

setDayOfWeek :: Calendar -> Int -> Calendar
setDayOfWeek cal = set1 cal DayOfWeek

dayOfWeekInMonth :: Calendar -> Int
dayOfWeekInMonth cal = unsafePerformIO $ getField cal DayOfWeekInMonth

setDayOfWeekInMonth :: Calendar -> Int -> Calendar
setDayOfWeekInMonth cal = set1 cal DayOfWeekInMonth

amPm :: Calendar -> Int
amPm cal = unsafePerformIO $ getField cal AmPm

setAmPm :: Calendar -> Int -> Calendar
setAmPm cal = set1 cal AmPm

hour :: Calendar -> Int
hour cal = unsafePerformIO $ getField cal Hour

setHour :: Calendar -> Int -> Calendar
setHour cal = set1 cal Hour

hourOfDay :: Calendar -> Int
hourOfDay cal = unsafePerformIO $ getField cal HourOfDay

setHourOfDay :: Calendar -> Int -> Calendar
setHourOfDay cal = set1 cal HourOfDay

minute :: Calendar -> Int
minute cal = unsafePerformIO $ getField cal Minute

setMinute :: Calendar -> Int -> Calendar
setMinute cal = set1 cal Minute

second :: Calendar -> Int
second cal = unsafePerformIO $ getField cal Second

setSecond :: Calendar -> Int -> Calendar
setSecond cal = set1 cal Second

millisecond :: Calendar -> Int
millisecond cal = unsafePerformIO $ getField cal Millisecond

setMillisecond :: Calendar -> Int -> Calendar
setMillisecond cal = set1 cal Millisecond

zoneOffset :: Calendar -> Int
zoneOffset cal = unsafePerformIO $ getField cal ZoneOffset

setZoneOffset :: Calendar -> Int -> Calendar
setZoneOffset cal = set1 cal ZoneOffset

dstOffset :: Calendar -> Int
dstOffset cal = unsafePerformIO $ getField cal DstOffset

setDstOffset :: Calendar -> Int -> Calendar
setDstOffset cal = set1 cal DstOffset

yearWoY :: Calendar -> Int
yearWoY cal = unsafePerformIO $ getField cal YearWoY

setYearWoY :: Calendar -> Int -> Calendar
setYearWoY cal = set1 cal YearWoY

doWLocal :: Calendar -> Int
doWLocal cal = unsafePerformIO $ getField cal DoWLocal

setDoWLocal :: Calendar -> Int -> Calendar
setDoWLocal cal = set1 cal DoWLocal

extendedYear :: Calendar -> Int
extendedYear cal = unsafePerformIO $ getField cal ExtendedYear

setExtendedYear :: Calendar -> Int -> Calendar
setExtendedYear cal = set1 cal ExtendedYear

julianDay :: Calendar -> Int
julianDay cal = unsafePerformIO $ getField cal JulianDay

setJulianDay :: Calendar -> Int -> Calendar
setJulianDay cal = set1 cal JulianDay

millisecondsInDay :: Calendar -> Int
millisecondsInDay cal = unsafePerformIO $ getField cal MillisecondsInDay

setMillisecondsInDay :: Calendar -> Int -> Calendar
setMillisecondsInDay cal = set1 cal MillisecondsInDay

isLeapMonth :: Calendar -> Bool
isLeapMonth cal = 0 /= (unsafePerformIO $ getField cal IsLeapMonth)

-- Copied from the lens package in order not to generate a dependency.
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = sbt s <$> afb (sa s)
{-# INLINE lens #-}

_era :: Lens' Calendar Int
_era = lens era setEra

_year :: Lens' Calendar Int
_year = lens year setYear

_month :: Lens' Calendar Int
_month = lens month setMonth

_dayOfYear :: Lens' Calendar Int
_dayOfYear = lens dayOfYear setDayOfYear

_dayOfMonth :: Lens' Calendar Int
_dayOfMonth = lens dayOfMonth setDayOfMonth

_dayOfWeek :: Lens' Calendar Int
_dayOfWeek = lens dayOfWeek setDayOfWeek

_dayOfWeekInMonth :: Lens' Calendar Int
_dayOfWeekInMonth = lens dayOfWeekInMonth setDayOfWeekInMonth

_amPm :: Lens' Calendar Int
_amPm = lens amPm setAmPm

_hour :: Lens' Calendar Int
_hour = lens hour setHour

_hourOfDay :: Lens' Calendar Int
_hourOfDay = lens hour setHourOfDay

_minute :: Lens' Calendar Int
_minute = lens minute setMinute

_second :: Lens' Calendar Int
_second = lens second setSecond

_millisecond :: Lens' Calendar Int
_millisecond = lens millisecond setMillisecond

_zoneOffset :: Lens' Calendar Int
_zoneOffset = lens zoneOffset setZoneOffset

_dstOffset :: Lens' Calendar Int
_dstOffset = lens dstOffset setDstOffset

_day :: Lens' Calendar Cal.Day
_day = lens day setDay

instance Show Calendar where
  show cal = show (utcTime cal)

foreign import ccall unsafe "hs_text_icu.h __hs_ucal_open" ucal_open
    :: Ptr UChar -> Int32 -> CString -> UCalendar -> Ptr UErrorCode
    -> IO (Ptr UCalendar)
foreign import ccall unsafe "hs_text_icu.h __hs_ucal_clone" ucal_clone
    :: Ptr UCalendar -> Ptr UErrorCode
    -> IO (Ptr UCalendar)
foreign import ccall unsafe "hs_text_icu.h &__hs_ucal_close" ucal_close
    :: FunPtr (Ptr UCalendar -> IO ())
foreign import ccall unsafe "hs_text_icu.h __hs_ucal_get" ucal_get
    :: Ptr UCalendar -> UCalendarDateFields -> Ptr UErrorCode
    -> IO Int32
foreign import ccall unsafe "hs_text_icu.h __hs_ucal_set" ucal_set
    :: Ptr UCalendar -> UCalendarDateFields -> Int32
    -> IO ()
foreign import ccall unsafe "hs_text_icu.h __hs_ucal_setDate" ucal_setDate
    :: Ptr UCalendar -> Int32 -> Int32 -> Int32
    -> Ptr UErrorCode -> IO ()
foreign import ccall unsafe "hs_text_icu.h __hs_ucal_setDateTime" ucal_setDateTime
    :: Ptr UCalendar -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32
    -> Ptr UErrorCode -> IO ()
foreign import ccall unsafe "hs_text_icu.h __hs_ucal_add" ucal_add
    :: Ptr UCalendar -> UCalendarDateFields -> Int32 -> Ptr UErrorCode
    -> IO ()
foreign import ccall unsafe "hs_text_icu.h __hs_ucal_roll" ucal_roll
    :: Ptr UCalendar -> UCalendarDateFields -> Int32 -> Ptr UErrorCode
    -> IO ()
foreign import ccall unsafe "hs_text_icu.h _hs__ucal_openTimeZoneIDEnumeration" ucal_openTimeZoneIDEnumeration
    :: USystemTimeZoneType -> Ptr UErrorCode
    -> IO (Ptr UEnumerator)
foreign import ccall unsafe "hs_text_icu.h __hs_ucal_openTimeZones" ucal_openTimeZones
    :: Ptr UErrorCode
    -> IO (Ptr UEnumerator)
foreign import ccall unsafe "hs_text_icu.h __hs_ucal_setTimeZone" ucal_setTimeZone
    :: Ptr UCalendar -> CString -> Int32 -> Ptr UErrorCode
    -> IO ()

{-# LANGUAGE ImportQualifiedPost, RankNTypes, BangPatterns, ForeignFunctionInterface, RecordWildCards #-}
-- |
-- Module      : Data.Text.ICU.Calendar
-- Copyright   : (c) 2010 Bryan O'Sullivan
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
      CalendarType(..), SystemTimeZoneType(..),
      -- * High-level interface
      -- ** Time zone functions
      timeZoneIDs, timeZones,
      -- ** Calendar field getters
      era, year, month, 
      dayOfMonth, dayOfYear, dayOfYear, dayOfWeek, dayOfWeekInMonth, amPm, 
      hour, minute, second, millisecond, zoneOffset, dstOffset, yearWoY, doWLocal, extendedYear, julianDay, 
      millisecondsInDay, isLeapMonth, day,
      -- ** Calendar field setters
      setEra, setYear, setMonth, 
      setDayOfMonth, setDayOfYear, setDayOfYear, setDayOfWeek, setDayOfWeekInMonth, setAmPm, 
      setHour, setMinute, setSecond, setMillisecond, setZoneOffset, setDstOffset, setYearWoY, setDoWLocal, setExtendedYear, setJulianDay, 
      setMillisecondsInDay, setDay,
      -- ** Lenses
      _era, _year, _month, 
      _dayOfMonth, _dayOfYear, _dayOfYear, _dayOfWeek, _dayOfWeekInMonth, _amPm, 
      _hour, _minute, _second, _millisecond, _zoneOffset, _dstOffset, _day,
      -- ** Calendar functions
      roll, add, set1, set,
      -- * Low-level interface
      open, openTimeZoneIDEnumeration, openTimeZones, CalendarDateField(..), getField, setField, setDate, rollField, addField
    ) where

#include <unicode/ucal.h>

import Control.DeepSeq (NFData(..))
import Control.Monad (forM_)
import Data.IORef (newIORef, writeIORef)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Text.Foreign (I16, useAsPtr)
import Data.Text.ICU.Calendar.Types
import Data.Text.ICU.Enumerator
import Data.Text.ICU.Error.Internal (UErrorCode, handleError)
import Data.Text.ICU.Internal (LocaleName(..), UBool, UChar, asBool, withLocaleName)
import Data.Time.Calendar qualified as Cal
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt(..))
import Foreign.ForeignPtr (newForeignPtr, withForeignPtr)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import Prelude hiding (last)
import System.IO.Unsafe (unsafePerformIO)

data CalendarDateField = 
  Era | Year | Month | 
  WeekOfYear | WeekOfMonth | 
  DayOfMonth | DayOfYear | DayOfWeek | DayOfWeekInMonth |
  AmPm | Hour | HourOfDay | Minute | Second | Millisecond |
  ZoneOffset | DstOffset | YearWoY | DoWLocal | ExtendedYear | JulianDay | 
  MillisecondsInDay | IsLeapMonth
  deriving (Show, Read, Eq)

type UCalendarDateFields = CInt

toUCalendarDateFields :: CalendarDateField -> UCalendarDateFields
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

-- | Open a UCalendar.
--
-- A UCalendar may be used to convert a millisecond value to a year, 
-- month, and day.
--
-- Note: When unknown TimeZone ID is specified or if the TimeZone ID 
-- specified is "Etc/Unknown", the UCalendar returned by the function 
-- is initialized with GMT zone with TimeZone ID UCAL_UNKNOWN_ZONE_ID 
-- ("Etc/Unknown") without any errors/warnings. If you want to check 
-- if a TimeZone ID is valid prior to this function, use 
-- ucal_getCanonicalTimeZoneID.
open :: Text -> LocaleName -> CalendarType -> IO Calendar
open zoneId loc typ =
  withLocaleName loc $ \locale ->
    useAsPtr zoneId $ \zoneIdPtr zoneIdLen -> do
      c <- handleError $ ucal_open zoneIdPtr (fromIntegral zoneIdLen) locale (toUCalendarType typ)
      calPtr <- newForeignPtr ucal_close c
      pure $ Calendar calPtr

clone :: Calendar -> IO Calendar
clone cal =
  withForeignPtr (calendarForeignPtr cal) $ \calPtr -> do
    c <- handleError $ ucal_clone calPtr
    calPtr <- newForeignPtr ucal_close c
    pure $ Calendar calPtr

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
openTimeZoneIDEnumeration typ = do
  e <- handleError $ ucal_openTimeZoneIDEnumeration (toUSystemTimeZoneType typ)
  createEnumerator e

-- | Create an enumeration over all time zones.
openTimeZones :: IO Enumerator
openTimeZones = do
  e <- handleError $ ucal_openTimeZones
  createEnumerator e

-- c <- open (pack "CET") (Locale "de_DE") TraditionalCalendarType
-- getField c Year
getField :: Calendar -> CalendarDateField -> IO Int
getField cal fld = withForeignPtr (calendarForeignPtr cal) $ \calPtr -> do
  n <- handleError $ ucal_get calPtr (toUCalendarDateFields fld)
  pure (fromIntegral n)

setField :: Calendar -> CalendarDateField -> Int -> IO ()
setField cal fld n = withForeignPtr (calendarForeignPtr cal) $ \calPtr -> 
  ucal_set calPtr (toUCalendarDateFields fld) (fromIntegral n)

setDate :: Calendar -> Int -> Int -> Int -> IO ()
setDate cal y m d = withForeignPtr (calendarForeignPtr cal) $ \calPtr -> 
  ucal_setDate calPtr (fromIntegral y) (fromIntegral m) (fromIntegral d)

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
rollField :: Calendar -> CalendarDateField -> Int -> IO ()
rollField cal fld n = withForeignPtr (calendarForeignPtr cal) $ \calPtr -> 
  handleError $ ucal_roll calPtr (toUCalendarDateFields fld) (fromIntegral n)

-- | Add a specified signed amount to a particular field in a Calendar.
--
-- See 'rollField' for further details.
roll :: Calendar -> [(CalendarDateField, Int)] -> Calendar
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
addField :: Calendar -> CalendarDateField -> Int -> IO ()
addField cal fld n = withForeignPtr (calendarForeignPtr cal) $ \calPtr -> 
  handleError $ ucal_add calPtr (toUCalendarDateFields fld) (fromIntegral n)

-- | Add a specified signed amount to a particular field in a Calendar.
--
-- See 'addField' for further details and see 'rollField' for differences 
-- compared to rolling.
add :: Calendar -> [(CalendarDateField, Int)] -> Calendar
add cal lst = unsafePerformIO $ do
  cal' <- clone cal
  forM_ lst (\(fld,n) -> addField cal' fld n)
  pure cal'

set1 :: Calendar -> CalendarDateField -> Int -> Calendar
set1 cal fld n = unsafePerformIO $ do
  cal' <- clone cal
  setField cal' fld n
  pure cal'

set :: Calendar -> [(CalendarDateField, Int)] -> Calendar
set cal lst = unsafePerformIO $ do
  cal' <- clone cal
  forM_ lst (\(fld,n) -> setField cal' fld n)
  pure cal'

day :: Calendar -> Cal.Day
day cal = unsafePerformIO $ do
  y <- getField cal Year
  m <- getField cal Month
  d <- getField cal DayOfMonth
  pure (Cal.fromGregorian (fromIntegral y) (m+1) d)

setDay :: Calendar -> Cal.Day -> Calendar
setDay cal aDay = unsafePerformIO $ do
  cal' <- clone cal
  let (y,m,d) = Cal.toGregorian aDay
  setDate cal' (fromIntegral y) (m-1) d
  pure cal'

era :: Calendar -> Int
era cal = unsafePerformIO $ getField cal Era

setEra :: Calendar -> Int -> Calendar
setEra cal = set1 cal Era

year :: Calendar -> Int
year cal = unsafePerformIO $ getField cal Year

setYear :: Calendar -> Int -> Calendar
setYear cal = set1 cal Year

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
  show cal = show (year cal) ++ "-" ++ show (1 + month cal) ++ "-" ++ show (dayOfMonth cal) ++ 
             " " ++ show (hourOfDay cal) ++ ":" ++ show (minute cal) ++ ":" ++ show (second cal)

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
    -> IO ()
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

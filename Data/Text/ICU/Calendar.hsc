{-# LANGUAGE BangPatterns, ForeignFunctionInterface, RecordWildCards #-}
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
      CalendarType(..),
      -- * High-level interface
      timeZones, 
      -- * Low-level interface
      open, openTimeZones
    ) where

#include <unicode/ucal.h>

import Control.DeepSeq (NFData(..))
import Control.Monad (forM)
import Data.IORef (newIORef, writeIORef)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Text.Foreign (I16, useAsPtr)
import Data.Text.ICU.Calendar.Types
import Data.Text.ICU.Enumerator
import Data.Text.ICU.Error.Internal (UErrorCode, handleError)
import Data.Text.ICU.Internal (LocaleName(..), UBool, UChar, asBool, withLocaleName)
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt(..))
import Foreign.ForeignPtr (newForeignPtr, withForeignPtr)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import Prelude hiding (last)
import System.IO.Unsafe (unsafePerformIO)

data CalendarType = TraditionalCalendarType | DefaultCalendarType | GregorianCalendarType
  deriving (Show, Read, Eq)

type UCalendarType = CInt

toUCalendarType :: CalendarType -> UCalendarType
toUCalendarType TraditionalCalendarType = #const UCAL_TRADITIONAL
toUCalendarType DefaultCalendarType = #const UCAL_TRADITIONAL
toUCalendarType GregorianCalendarType = #const UCAL_GREGORIAN

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
      r <- newIORef zoneId
      calPtr <- newForeignPtr ucal_close c
      pure $ CAL r loc calPtr

-- | List of all time zones.
timeZones :: IO [Text]
timeZones = do
  tzEnum <- openTimeZones
  tzs <- toList tzEnum
  pure tzs

-- | Create an enumeration over all time zones.
openTimeZones :: IO Enumerator
openTimeZones = do
  e <- handleError $ ucal_openTimeZones
  createEnumerator e

foreign import ccall unsafe "hs_text_icu.h __hs_ucal_open" ucal_open
    :: Ptr UChar -> Int32 -> CString -> UCalendar -> Ptr UErrorCode
    -> IO (Ptr UCalendar)
foreign import ccall unsafe "hs_text_icu.h &__hs_ucal_close" ucal_close
    :: FunPtr (Ptr UCalendar -> IO ())
foreign import ccall unsafe "hs_text_icu.h __hs_ucal_openTimeZones" ucal_openTimeZones
    :: Ptr UErrorCode
    -> IO (Ptr UEnumerator)

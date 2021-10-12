{-# LANGUAGE EmptyDataDecls #-}

-- |
-- Module      : Data.Text.ICU.Calendar.Internal
-- Copyright   : (c) 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
module Data.Text.ICU.Calendar.Types
  ( Calendar (..),
    UCalendar,
  )
where

import Data.IORef (IORef)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Text.ICU.Internal (LocaleName (..))
import Foreign.C.Types (CInt (..))
import Foreign.ForeignPtr (ForeignPtr)

type UCalendar = CInt

-- A calendar
data Calendar = Calendar {calendarForeignPtr :: ForeignPtr UCalendar}

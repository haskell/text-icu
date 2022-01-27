{-# LANGUAGE DeriveDataTypeable, ForeignFunctionInterface, EmptyDataDecls #-}
-- |
-- Module      : Data.Text.ICU.CharsetDetection.Internal
-- Copyright   : (c) 2017 Zac Slade
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Access to the Unicode Character Set Detection facilities, implemented in
-- the International Components for Unicode (ICU) libraries.
--
-- For more information see the \"Character Set Detection\" chapter
-- in the ICU User Guide
-- <http://userguide.icu-project.org/conversion/detection>.
module Data.Text.ICU.CharsetDetection.Internal
    (
      UCharsetDetector
    , UCharsetMatch
    , CharsetMatch(..)
    , CharsetDetector(..)
    , withCharsetDetector
    , wrapUCharsetDetector
    , wrapUCharsetMatch
    , mkCharsetDetector
    , withCharsetMatch
    ) where

import Control.Exception (mask_)
import Data.Typeable (Typeable)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, newForeignPtr_, withForeignPtr)
import Foreign.Ptr (FunPtr, Ptr)

import Data.Text.ICU.Error.Internal (UErrorCode, handleError)

#include <unicode/ucsdet.h>

-- | Opaque handle to a character set detector
data UCharsetDetector

-- | Handy wrapper for the pointer to the 'UCharsetDetector'. We must
-- always call ucsdet_close on any UCharsetDetector when we are done. The
-- 'withCharsetDetector' and 'wrapUCharsetDetector' functions simplify
-- management of the pointers.
data CharsetDetector = CharsetDetector {
    charsetDetectorPtr :: {-# UNPACK #-} !(ForeignPtr UCharsetDetector)
} deriving (Typeable)

mkCharsetDetector :: IO CharsetDetector
mkCharsetDetector = wrapUCharsetDetector $ handleError ucsdet_open

-- | Temporarily unwraps an 'CharsetDetector' to perform operations on its
-- raw 'UCharsetDetector' handle.
withCharsetDetector :: CharsetDetector -> (Ptr UCharsetDetector -> IO a) -> IO a
withCharsetDetector (CharsetDetector ucsd) = withForeignPtr ucsd
{-# INLINE withCharsetDetector #-}

-- | Wraps a raw 'UCharsetDetector' in an 'CharsetDetector', closing the
-- handle when the last reference to the object is dropped.
wrapUCharsetDetector :: IO (Ptr UCharsetDetector) -> IO CharsetDetector
wrapUCharsetDetector a = mask_ $ fmap CharsetDetector $ newForeignPtr ucsdet_close =<< a
{-# INLINE wrapUCharsetDetector #-}

-- | Opaque handle to a character set match
data UCharsetMatch

-- | Opaque character set match handle. The memory backing these objects is
-- managed entirely by the ICU C library.
data CharsetMatch = CharsetMatch {
    charsetMatchPtr :: {-# UNPACK #-} !(ForeignPtr UCharsetMatch)
} deriving (Typeable)

wrapUCharsetMatch :: IO (Ptr UCharsetMatch) -> IO CharsetMatch
wrapUCharsetMatch a = mask_ $ fmap CharsetMatch $ newForeignPtr_ =<< a

withCharsetMatch :: CharsetMatch -> (Ptr UCharsetMatch -> IO a) -> IO a
withCharsetMatch (CharsetMatch ucsm) = withForeignPtr ucsm

foreign import ccall unsafe "hs_text_icu.h &__hs_ucsdet_close" ucsdet_close
    :: FunPtr (Ptr UCharsetDetector -> IO ())

foreign import ccall unsafe "hs_text_icu.h __hs_ucsdet_open" ucsdet_open
    :: Ptr UErrorCode -> IO (Ptr UCharsetDetector)

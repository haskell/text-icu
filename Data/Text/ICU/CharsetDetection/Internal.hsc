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
    ,UCharsetMatch
    ,MCharsetDetector(..)
    ,CharsetMatch(..)
    ,CharsetDetector(..)
    ,withCharsetDetector
    ,wrapUCharsetDetector
    ) where

import Data.Typeable (Typeable)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Ptr (FunPtr, Ptr)

#include <unicode/ucsdet.h>

-- | Opaque handle to a character set detector
data UCharsetDetector

-- | Handy wrapper for the pointer to the 'UCharsetDetector'. We must
-- always call ucsdet_close on any UCharsetDetector when we are done. The
-- 'withCharsetDetector' and 'wrapUCharsetDetector' functions simplify
-- management of the pointers.
data MCharsetDetector = MCharsetDetector {
    charsetDetectorPtr :: {-# UNPACK #-} !(ForeignPtr UCharsetDetector)
} deriving (Typeable)

-- | Charset detector type
newtype CharsetDetector = CD MCharsetDetector
    deriving (Typeable)

-- | Temporarily unwraps an 'MCharsetDetector' to perform operations on its
-- raw 'UCharsetDetector' handle.
withCharsetDetector :: MCharsetDetector -> (Ptr UCharsetDetector -> IO a) -> IO a
withCharsetDetector (MCharsetDetector ucsd) = withForeignPtr ucsd
{-# INLINE withCharsetDetector #-}

-- | Wraps a raw 'UCharsetDetector' in an 'MCharsetDetector', closing the
-- handle when the last reference to the object is dropped.
wrapUCharsetDetector :: Ptr UCharsetDetector -> IO MCharsetDetector
wrapUCharsetDetector = fmap MCharsetDetector . newForeignPtr ucsdet_close
{-# INLINE wrapUCharsetDetector #-}

-- | Opaque handle to a character set match
data UCharsetMatch

-- | Opaque character set match handle. The memory backing these objects is
-- managed entirely by the ICU C library.
data CharsetMatch = CharsetMatch {
    charsetMatchPtr :: {-# UNPACK #-} !(ForeignPtr UCharsetMatch)
} deriving (Typeable)

foreign import ccall unsafe "hs_text_icu.h &__hs_ucsdet_close" ucsdet_close
    :: FunPtr (Ptr UCharsetDetector -> IO ())

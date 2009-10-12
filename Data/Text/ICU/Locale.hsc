{-# LANGUAGE CPP, DeriveDataTypeable, ForeignFunctionInterface,
    GeneralizedNewtypeDeriving #-}
module Data.Text.ICU.Locale
    (
    -- * Types
      Locale
    , Layout(..)
    -- * Functions
    , locale
    , localeName
    , available
    , getDefault
    , setDefault
    -- ** Layout
    , characterOrientation
    , lineOrientation
    -- * Constants
    -- ** Languages / macrolanguages
    , arabic
    , bengali
    , chinese
    , english
    , french
    , german
    , hindi
    , italian
    , japanese
    , korean
    , portuguese
    , russian
    , simplifiedChinese
    , spanish
    , traditionalChinese
    -- ** Countries and regions
    , canada
    , canadaFrench
    , china
    , prc
    , france
    , germany
    , italy
    , japan
    , korea
    , taiwan
    , uk
    , us
    )
    where

#include <unicode/uloc.h>

import Control.Exception (throw, try)
import Data.Int (Int32)
import Data.Text.ICU.Error.Codes (ErrorCode, UErrorCode)
import Data.Text.ICU.Error.Internal (handleError, preflight)
import Foreign.C.String (CString, peekCAString, withCAString)
import Foreign.C.Types (CInt)
import Foreign.Ptr (Ptr, nullPtr)
import System.IO.Unsafe (unsafePerformIO)

newtype Locale = Locale { localeName :: String }
    deriving (Eq, Ord, Show)

data Layout = LeftToRight
            | RightToLeft
            | TopToBottom
            | BottomToTop
            | UnknownLayout
              deriving (Eq, Ord, Enum, Bounded, Show)

-- | A list of the available locales,
-- e.g. @["af","af_NA","af_ZA","am","am_ET",...]@.
available :: [Locale]
available = unsafePerformIO $ do
  n <- countAvailable
  mapM getAvailable [0..n-1]

getAvailable :: (Integral a) => a -> IO Locale
getAvailable n = (fmap Locale . peekCAString) =<< uloc_getAvailable (fromIntegral n)

countAvailable :: IO Int
countAvailable = fromIntegral `fmap` uloc_countAvailable

-- | Get ICU's default locale.
getDefault :: IO Locale
getDefault = fmap Locale . peekCAString =<< uloc_getDefault

-- | Sets ICU's default locale.  If 'Nothing', reset to the system's
-- default locale.  Otherwise, use the given locale.
--
-- By default (without calling this function), ICU's default locale
-- will be based on information obtained from the underlying system
-- environment.
--
-- Changes to ICU's default locale do not propagate back to the system
-- environment.
--
-- Changes to ICU's default locale will not affect any ICU services
-- that may already be open based on the previous default locale
-- value.
setDefault :: Maybe Locale -> IO ()
setDefault (Just (Locale loc)) =
    withCAString loc (handleError . uloc_setDefault)
setDefault Nothing = handleError . uloc_setDefault $ nullPtr

-- | Construct the named locale.
--
-- Note: This has the effect of \"canonicalizing\" the ICU locale ID
-- to a certain extent. Upper and lower case are set as needed. This
-- function does /not/ map aliased names in any way.
locale :: String -> Either ErrorCode Locale
locale loc = unsafePerformIO . withCAString loc $
             (try . fmap Locale . preflight 16 . uloc_getName)

-- | Get the layout character orientation for the specified locale.
characterOrientation :: Locale -> Either ErrorCode Layout
characterOrientation (Locale loc) =
    unsafePerformIO . withCAString loc $
    (try . fmap (toEnum . fromIntegral) . handleError . uloc_getCharacterOrientation)

-- | Get the layout line orientation for the specified locale.
lineOrientation :: Locale -> Either ErrorCode Layout
lineOrientation (Locale loc) =
    unsafePerformIO . withCAString loc $
    (try . fmap (toEnum . fromIntegral) . handleError . uloc_getLineOrientation)

defaultLocale :: String -> Locale
defaultLocale s = case locale s of
                    Left err -> unsafePerformIO $ throw err
                    Right l  -> l

arabic :: Locale
arabic = defaultLocale "ar"

bengali :: Locale
bengali = defaultLocale "bn"

chinese :: Locale
chinese = defaultLocale "zh"

english :: Locale
english = defaultLocale "en"

french :: Locale
french = defaultLocale "fr"

german :: Locale
german = defaultLocale "de"

hindi :: Locale
hindi = defaultLocale "hi"

italian :: Locale
italian = defaultLocale "it"

japanese :: Locale
japanese = defaultLocale "ja"

korean :: Locale
korean = defaultLocale "ko"

portuguese :: Locale
portuguese = defaultLocale "pt"

russian :: Locale
russian = defaultLocale "ru"

simplifiedChinese :: Locale
simplifiedChinese = defaultLocale "zh_CN"

spanish :: Locale
spanish = defaultLocale "es"

traditionalChinese :: Locale
traditionalChinese = defaultLocale "zh_TW"

canada :: Locale
canada = defaultLocale "en_CA"

canadaFrench :: Locale
canadaFrench = defaultLocale "fr_CA"

china :: Locale
china = defaultLocale "zh_CN"

prc :: Locale
prc = china

france :: Locale
france = defaultLocale "fr_FR"

germany :: Locale
germany = defaultLocale "de_DE"

italy :: Locale
italy = defaultLocale "it_IT"

japan :: Locale
japan = defaultLocale "ja_JP"

korea :: Locale
korea = defaultLocale "ko_KR"

taiwan :: Locale
taiwan = defaultLocale "zh_TW"

uk :: Locale
uk = defaultLocale "en_GB"

us :: Locale
us = defaultLocale "en_US"


foreign import ccall unsafe "unicode/uloc.h uloc_getAvailable_4_0" uloc_getAvailable
    :: Int32 -> IO CString
foreign import ccall unsafe "unicode/uloc.h uloc_countAvailable_4_0" uloc_countAvailable
    :: IO Int32
foreign import ccall unsafe "unicode/uloc.h uloc_getDefault_4_0" uloc_getDefault
    :: IO CString
foreign import ccall unsafe "unicode/uloc.h uloc_setDefault_4_0" uloc_setDefault
    :: CString -> Ptr UErrorCode -> IO ()
foreign import ccall unsafe "unicode/uloc.h uloc_getName_4_0" uloc_getName
    :: CString -> CString -> Int32 -> Ptr UErrorCode -> IO Int32
foreign import ccall unsafe "unicode/uloc.h uloc_getCharacterOrientation_4_0" uloc_getCharacterOrientation
    :: CString -> Ptr UErrorCode -> IO CInt
foreign import ccall unsafe "unicode/uloc.h uloc_getLineOrientation_4_0" uloc_getLineOrientation
    :: CString -> Ptr UErrorCode -> IO CInt

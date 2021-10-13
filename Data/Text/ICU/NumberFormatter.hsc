{-# LANGUAGE EmptyDataDecls, BlockArguments, ImportQualifiedPost, RankNTypes, BangPatterns, ForeignFunctionInterface, RecordWildCards #-}
-- |
-- Module      : Data.Text.ICU.NumberFormatter
-- Copyright   : (c) 2021 Torsten Kemps-Benedix
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Number formatter implemented as bindings to
-- the International Components for Unicode (ICU) libraries.

module Data.Text.ICU.NumberFormatter
    (NumberFormatter, numberFormatter, formatIntegral, formatIntegral', formatDouble, formatDouble'
    ) where

#include <unicode/unumberformatter.h>

import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Text.Foreign (useAsPtr, fromPtr)
import Data.Text.ICU.Error.Internal (UErrorCode, handleError, handleOverflowError)
import Data.Text.ICU.Internal (LocaleName(..), UChar, withLocaleName)
import Foreign.C.String (CString)
import Foreign.C.Types (CDouble(..))
import Foreign.ForeignPtr (newForeignPtr, withForeignPtr, ForeignPtr)
import Foreign.Ptr (FunPtr, Ptr, castPtr)
import Prelude hiding (last)
import System.IO.Unsafe (unsafePerformIO)

data UNumberFormatter
data UFormattedNumber

newtype NumberFormatter = NumberFormatter (ForeignPtr UNumberFormatter)

-- | Create a new 'NumberFormatter'.
--
-- See https://unicode-org.github.io/icu/userguide/format_parse/numbers/skeletons.html for how to specify
-- the number skeletons. And use 'availableLocales' in order to find the allowed locale names. These
-- usuallly look like "en", "de", "de_AT" etc. See 'formatIntegral' and 'formatDouble' for some examples.
numberFormatter :: Text -> LocaleName -> IO NumberFormatter
numberFormatter skel loc =
  withLocaleName loc $ \locale ->
    useAsPtr skel $ \skelPtr skelLen -> do
      nf <- handleError $ unumf_openForSkeletonAndLocale skelPtr (fromIntegral skelLen) locale
      nfPtr <- newForeignPtr unumf_close nf
      pure $ NumberFormatter nfPtr

-- | Format an integral number.
--
-- See https://unicode-org.github.io/icu/userguide/format_parse/numbers/skeletons.html for how to specify
-- the number skeletons.
--
-- >>> import Data.Text
-- >>> nf <- numberFormatter (pack "precision-integer") (Locale "de")
-- >>> formatIntegral nf 12345
-- "12.345"
-- >>> nf2 <- numberFormatter (pack "precision-integer") (Locale "fr")
-- >>> formatIntegral nf2 12345
-- "12\8239\&345"
formatIntegral :: (Integral a) => NumberFormatter -> a -> Text
formatIntegral (NumberFormatter nf) x = unsafePerformIO do
  withForeignPtr nf $ \nfPtr -> do
    result <- handleError $ unumf_openResult
    resultPtr <- newForeignPtr unumf_closeResult result
    withForeignPtr resultPtr $ \resPtr -> do
      handleError $ unumf_formatInt nfPtr (fromIntegral x) resPtr
      t <- handleOverflowError (fromIntegral (64 :: Int))
        (\dptr dlen -> unumf_resultToString resPtr dptr dlen)
        (\dptr dlen -> fromPtr (castPtr dptr) (fromIntegral dlen))
      pure t

-- | Create a number formatter and apply it to an integral number.
formatIntegral' :: (Integral a) => Text -> LocaleName -> a -> Text
formatIntegral' skel loc x = unsafePerformIO do
  nf <- numberFormatter skel loc
  pure $ formatIntegral nf x

-- | Format a Double.
--
-- See https://unicode-org.github.io/icu/userguide/format_parse/numbers/skeletons.html for how to specify
-- the number skeletons.
--
-- >>> import Data.Text
-- >>> nf3 <- numberFormatter (pack "precision-currency-cash") (Locale "it")
-- >>> formatDouble nf3 12345.6789
-- "12.345,68"
formatDouble :: NumberFormatter -> Double -> Text
formatDouble (NumberFormatter nf) x = unsafePerformIO do
  withForeignPtr nf $ \nfPtr -> do
    result <- handleError $ unumf_openResult
    resultPtr <- newForeignPtr unumf_closeResult result
    withForeignPtr resultPtr $ \resPtr -> do
      handleError $ unumf_formatDouble nfPtr (CDouble x) resPtr
      t <- handleOverflowError (fromIntegral (64 :: Int))
        (\dptr dlen -> unumf_resultToString resPtr dptr dlen)
        (\dptr dlen -> fromPtr (castPtr dptr) (fromIntegral dlen))
      pure t

-- | Create a number formatter and apply it to a Double.
formatDouble' :: Text -> LocaleName -> Double -> Text
formatDouble' skel loc x = unsafePerformIO do
  nf <- numberFormatter skel loc
  pure $ formatDouble nf x

foreign import ccall unsafe "hs_text_icu.h __hs_unumf_openForSkeletonAndLocale" unumf_openForSkeletonAndLocale
    :: Ptr UChar -> Int32 -> CString -> Ptr UErrorCode -> IO (Ptr UNumberFormatter)
foreign import ccall unsafe "hs_text_icu.h &__hs_unumf_close" unumf_close
    :: FunPtr (Ptr UNumberFormatter -> IO ())
foreign import ccall unsafe "hs_text_icu.h __hs_unumf_openResult" unumf_openResult
    :: Ptr UErrorCode -> IO (Ptr UFormattedNumber)
foreign import ccall unsafe "hs_text_icu.h &__hs_unumf_closeResult" unumf_closeResult
    :: FunPtr (Ptr UFormattedNumber -> IO ())
foreign import ccall unsafe "hs_text_icu.h __hs_unumf_formatInt" unumf_formatInt
    :: Ptr UNumberFormatter -> Int64 -> Ptr UFormattedNumber -> Ptr UErrorCode -> IO ()
foreign import ccall unsafe "hs_text_icu.h __hs_unumf_formatDouble" unumf_formatDouble
    :: Ptr UNumberFormatter -> CDouble -> Ptr UFormattedNumber -> Ptr UErrorCode -> IO ()
foreign import ccall unsafe "hs_text_icu.h __hs_unumf_resultToString" unumf_resultToString
    :: Ptr UFormattedNumber -> Ptr UChar -> Int32 -> Ptr UErrorCode -> IO Int32

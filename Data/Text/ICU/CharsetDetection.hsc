{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : Data.Text.ICU.CharsetDetection
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
module Data.Text.ICU.CharsetDetection
    (
      setText
    , detect
    , mkCharsetDetector
    , withCharsetDetector
    , wrapUCharsetMatch
    , CharsetMatch
    , CharsetDetector
    , getConfidence
    , getName
    , getLanguage
    ) where

import Foreign.Ptr (Ptr)
import Foreign.C.String (CString)
import Foreign.C.Types (CChar)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.Text.Encoding as TE
import Data.Text (Text)

import Data.Text.ICU.Error.Internal (UErrorCode, handleError)
import Data.Text.ICU.CharsetDetection.Internal (UCharsetMatch, UCharsetDetector,
                                                CharsetDetector, CharsetMatch,
                                                mkCharsetDetector,
                                                withCharsetDetector,
                                                withCharsetMatch,
                                                wrapUCharsetMatch)

#include <unicode/ucsdet.h>

-- | From the ICU C API documentation:
-- "Character set detection is at best an imprecise operation. The
-- detection process will attempt to identify the charset that best matches
-- the characteristics of the byte data, but the process is partly statistical
-- in nature, and the results can not be guaranteed to always be correct.
--
-- For best accuracy in charset detection, the input data should be primarily
-- in a single language, and a minimum of a few hundred bytes worth of plain
-- text in the language are needed. The detection process will attempt to
-- ignore html or xml style markup that could otherwise obscure the content."

-- | Use the first 512 bytes, if available, as the text in the
-- 'CharsetDetector' object. This function is low-level and used by the more
-- high-level 'detect' function.
setText :: ByteString -> CharsetDetector -> IO ()
setText bs ucsd = withCharsetDetector ucsd go
  where
    go u = if BS.length bs < 512
              then BS.useAsCStringLen bs (\(text,size) -> handleError $ ucsdet_setText u text size)
              else BS.useAsCStringLen (BS.take 512 bs) (\(text,size) -> handleError $ ucsdet_setText u text size)

-- | Attempt to perform a detection without an input filter. The best match
-- will be returned.
detect :: ByteString -> IO CharsetMatch
detect bs = do
    ucsd <- mkCharsetDetector
    setText bs ucsd
    wrapUCharsetMatch ucsd $ withCharsetDetector ucsd (handleError . ucsdet_detect)

-- | See the confidence score from 0-100 of the 'CharsetMatch' object.
getConfidence :: CharsetMatch -> IO Int
getConfidence ucm = withCharsetMatch ucm $ handleError . ucsdet_getConfidence

-- | Extract the character set encoding name from the 'CharsetMatch'
-- object.
getName :: CharsetMatch -> IO Text
getName ucsm = do
    bs <- withCharsetMatch ucsm (handleError . ucsdet_getName) >>= BS.packCString
    return $ TE.decodeUtf8 bs

-- | Extracts the three letter ISO code for the language encoded in the
-- 'CharsetMatch'.
getLanguage :: CharsetMatch -> IO Text
getLanguage ucsm = do
    bs <- withCharsetMatch ucsm (handleError . ucsdet_getLanguage) >>= BS.packCString
    return $ TE.decodeUtf8 bs

foreign import ccall unsafe "hs_text_icu.h __hs_ucsdet_setText" ucsdet_setText
    :: Ptr UCharsetDetector -> Ptr CChar -> Int -> Ptr UErrorCode -> IO ()

foreign import ccall unsafe "hs_text_icu.h __hs_ucsdet_detect" ucsdet_detect
    :: Ptr UCharsetDetector -> Ptr UErrorCode -> IO (Ptr UCharsetMatch)

foreign import ccall unsafe "hs_text_icu.h __hs_ucsdet_getConfidence" ucsdet_getConfidence
    :: Ptr UCharsetMatch -> Ptr UErrorCode -> IO Int

foreign import ccall unsafe "hs_text_icu.h __hs_ucsdet_getName" ucsdet_getName
    :: Ptr UCharsetMatch -> Ptr UErrorCode -> IO CString

foreign import ccall unsafe "hs_text_icu.h __hs_ucsdet_getLanguage" ucsdet_getLanguage
    :: Ptr UCharsetMatch -> Ptr UErrorCode -> IO CString

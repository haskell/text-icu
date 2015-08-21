{-# LANGUAGE BangPatterns, CPP, DeriveDataTypeable, ForeignFunctionInterface, OverloadedStrings #-}
-- |
-- Module      : Data.Text.ICU.Spoof
-- Copyright   : (c) 2015 Ben Hamilton
--
-- License     : BSD-style
-- Maintainer  : bgertzfield@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- String spoofing (confusability) checks for Unicode, implemented as bindings to
-- the International Components for Unicode (ICU) uspoof library.

module Data.Text.ICU.Spoof
    (
    -- * Unicode spoof checking API
    -- $api
    -- * Types
      MSpoof
    , Spoof
    , SpoofCheck(..)
    , SpoofCheckResult(..)
    , RestrictionLevel(..)
    , SkeletonTypeOverride(..)
    -- * Functions
    , open
    , openFromSerialized
    , openFromSource
    , getSkeleton
    , getChecks
    , setChecks
    , getRestrictionLevel
    , setRestrictionLevel
    , getAllowedLocales
    , setAllowedLocales
    , areConfusable
    , spoofCheck
    , serialize
    ) where

#include <unicode/uspoof.h>

import Control.Applicative
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString.Internal (create, memcpy, toForeignPtr)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Int (Int32)
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import Data.Text (Text, pack, splitOn, strip, unpack)
import Data.Text.Foreign (fromPtr, useAsPtr)
import Data.Text.ICU.BitMask (ToBitMask, fromBitMask, toBitMask)
import Data.Text.ICU.Spoof.Internal (MSpoof, USpoof, Spoof, withSpoof, wrap, wrapWithSerialized)
import Data.Text.ICU.Error.Internal (UErrorCode, handleError, handleOverflowError)
import Data.Text.ICU.Internal (LocaleName(..), UChar)
import Data.Word (Word8)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.ForeignPtr (withForeignPtr)

-- $api
--

data SpoofCheck = SingleScriptConfusable
                | MixedScriptConfusable
                | WholeScriptConfusable
                | AnyCase
                | RestrictionLevel
                | Invisible
                | CharLimit
                | MixedNumbers
                | AllChecks
                | AuxInfo
                deriving (Bounded, Enum, Eq, Show)

instance ToBitMask SpoofCheck where
  toBitMask SingleScriptConfusable = #const USPOOF_SINGLE_SCRIPT_CONFUSABLE
  toBitMask MixedScriptConfusable = #const USPOOF_MIXED_SCRIPT_CONFUSABLE
  toBitMask WholeScriptConfusable = #const USPOOF_WHOLE_SCRIPT_CONFUSABLE
  toBitMask AnyCase = #const USPOOF_ANY_CASE
  toBitMask RestrictionLevel = #const USPOOF_RESTRICTION_LEVEL
  toBitMask Invisible = #const USPOOF_INVISIBLE
  toBitMask CharLimit = #const USPOOF_CHAR_LIMIT
  toBitMask MixedNumbers = #const USPOOF_MIXED_NUMBERS
  toBitMask AllChecks = #const USPOOF_ALL_CHECKS
  toBitMask AuxInfo = #const USPOOF_AUX_INFO

type USpoofCheck = Int32

data RestrictionLevel = ASCII
                      | SingleScriptRestrictive
                      | HighlyRestrictive
                      | ModeratelyRestrictive
                      | MinimallyRestrictive
                      | Unrestrictive
                      deriving (Bounded, Enum, Eq, Show)

instance ToBitMask RestrictionLevel where
  toBitMask ASCII = #const USPOOF_ASCII
  toBitMask SingleScriptRestrictive = #const USPOOF_SINGLE_SCRIPT_RESTRICTIVE
  toBitMask HighlyRestrictive = #const USPOOF_HIGHLY_RESTRICTIVE
  toBitMask ModeratelyRestrictive = #const USPOOF_MODERATELY_RESTRICTIVE
  toBitMask MinimallyRestrictive = #const USPOOF_MINIMALLY_RESTRICTIVE
  toBitMask Unrestrictive = #const USPOOF_UNRESTRICTIVE

type URestrictionLevel = Int32

data SpoofCheckResult = CheckOK
                      | CheckFailed [SpoofCheck]
                      | CheckFailedWithRestrictionLevel { checks :: [SpoofCheck],
                                                           level :: RestrictionLevel }
                deriving (Eq, Show)

data SkeletonTypeOverride = SkeletonSingleScript
                          | SkeletonAnyCase
                          deriving (Bounded, Enum, Eq, Show)

instance ToBitMask SkeletonTypeOverride where
  toBitMask SkeletonSingleScript = #const USPOOF_SINGLE_SCRIPT_CONFUSABLE
  toBitMask SkeletonAnyCase = #const USPOOF_ANY_CASE

type USkeletonTypeOverride = Int32

makeSpoofCheckResult :: USpoofCheck -> SpoofCheckResult
makeSpoofCheckResult c =
  case spoofChecks of
    [] -> CheckOK
    _ ->
      case restrictionLevel of
        Nothing -> CheckFailed spoofChecks
        Just l -> CheckFailedWithRestrictionLevel spoofChecks l
  where spoofChecks = fromBitMask $ fromIntegral $ c .&. #const USPOOF_ALL_CHECKS
        restrictionValue = c .&. #const USPOOF_RESTRICTION_LEVEL_MASK
        restrictionLevel = listToMaybe $ fromBitMask $ fromIntegral $ restrictionValue

-- | Open a spoof checker for checking Unicode strings for lookalike security issues.
open :: IO MSpoof
open = wrap =<< handleError uspoof_open

-- | Open a spoof checker previously serialized to bytes using 'serialize'.
-- The ForeignPtr in the ByteString will remain referenced for the
-- lifetime of the returned object. The memory it points to must not
-- be modified until the returned object is closed.
openFromSerialized :: ByteString -> IO MSpoof
openFromSerialized b =
  case toForeignPtr b of
    (ptr, off, len) -> withForeignPtr ptr $ \p ->
      wrapWithSerialized ptr =<< handleError (uspoof_openFromSerialized (p `plusPtr` off) (fromIntegral len) nullPtr)

-- | Open a spoof checker given the contents of the "confusables.txt" and "confusablesWholeScript.txt"
-- files as described in Unicode UAX #39.
openFromSource :: ByteString -> ByteString -> IO MSpoof
openFromSource confusables confusablesWholeScript =
  unsafeUseAsCStringLen confusables $ \(cptr, clen) ->
    unsafeUseAsCStringLen confusablesWholeScript $ \(wptr, wlen) ->
      wrap =<< handleError (uspoof_openFromSource cptr (fromIntegral clen) wptr (fromIntegral wlen) nullPtr nullPtr)

-- | Get the checks performed by a spoof checker.
getChecks :: MSpoof -> IO SpoofCheckResult
getChecks s = do
  withSpoof s $ \sptr ->
    makeSpoofCheckResult <$> handleError (uspoof_getChecks sptr)

-- | Configure the checks performed by a spoof checker.
setChecks :: MSpoof -> [SpoofCheck] -> IO ()
setChecks s c = do
  withSpoof s $ \sptr ->
    handleError $ uspoof_setChecks sptr . fromIntegral $ toBitMask c

-- | Get the restriction level of a spoof checker.
getRestrictionLevel :: MSpoof -> IO (Maybe RestrictionLevel)
getRestrictionLevel s = do
  withSpoof s $ \sptr ->
    (listToMaybe . fromBitMask . fromIntegral) <$> uspoof_getRestrictionLevel sptr

-- | Configure the restriction level of a spoof checker.
setRestrictionLevel :: MSpoof -> RestrictionLevel -> IO ()
setRestrictionLevel s l = do
  withSpoof s $ \sptr ->
    uspoof_setRestrictionLevel sptr . fromIntegral $ toBitMask l

-- | Get the list of locales allowed to be used with a spoof checker.
getAllowedLocales :: MSpoof -> IO [LocaleName]
getAllowedLocales s = do
  withSpoof s $ \sptr ->
    splitLocales <$> (peekCString =<< (handleError (uspoof_getAllowedLocales sptr)))
    where splitLocales = fmap (Locale . unpack . strip) . splitOn "," . pack

-- | Get the list of locales allowed to be used with a spoof checker.
setAllowedLocales :: MSpoof -> [LocaleName] -> IO ()
setAllowedLocales s l = do
  withSpoof s $ \sptr ->
    withCString (joinLocales l) $ \lptr ->
      handleError (uspoof_setAllowedLocales sptr lptr)
    where joinLocales = intercalate "," . fmap show

-- | Check if two strings could be confused with each other.
areConfusable :: MSpoof -> Text -> Text -> IO SpoofCheckResult
areConfusable s t1 t2 = do
  withSpoof s $ \sptr ->
    useAsPtr t1 $ \t1ptr t1len ->
      useAsPtr t2 $ \t2ptr t2len ->
        makeSpoofCheckResult <$>
          handleError (uspoof_areConfusable sptr t1ptr (fromIntegral t1len) t2ptr (fromIntegral t2len))

-- | Generate a re-usable "skeleton" to check if an identifier is confusable
-- with some large set of existing identifiers.
getSkeleton :: MSpoof -> Maybe SkeletonTypeOverride -> Text -> IO Text
getSkeleton s o t = do
  withSpoof s $ \sptr ->
    useAsPtr t $ \tptr tlen ->
    handleOverflowError (fromIntegral tlen)
    (\dptr dlen -> (uspoof_getSkeleton sptr oflags tptr (fromIntegral tlen) dptr (fromIntegral dlen)))
    (\dptr dlen -> fromPtr (castPtr dptr) (fromIntegral dlen))
    where oflags = case o of
            Nothing -> 0
            Just typeOverride -> (fromIntegral $ toBitMask typeOverride)

-- | Check if a string could be confused with any other.
spoofCheck :: MSpoof -> Text -> IO SpoofCheckResult
spoofCheck s t = do
  withSpoof s $ \sptr ->
    useAsPtr t $ \tptr tlen ->
      makeSpoofCheckResult <$> handleError (uspoof_check sptr tptr (fromIntegral tlen) nullPtr)

-- | Serialize the rules in this spoof checker to memory, suitable for re-use
-- by openFromSerialized.
serialize :: MSpoof -> IO ByteString
serialize s = do
  withSpoof s $ \sptr ->
    handleOverflowError 0
    (\dptr dlen -> (uspoof_serialize sptr dptr (fromIntegral dlen)))
    (\dptr dlen -> create (fromIntegral dlen) $ \bptr -> memcpy dptr bptr (fromIntegral dlen))

foreign import ccall unsafe "hs_text_icu.h __hs_uspoof_open" uspoof_open
    :: Ptr UErrorCode -> IO (Ptr USpoof)

foreign import ccall unsafe "hs_text_icu.h __hs_uspoof_openFromSerialized" uspoof_openFromSerialized
    :: Ptr Word8 -> Int32 -> Ptr Int32 -> Ptr UErrorCode -> IO (Ptr USpoof)

foreign import ccall unsafe "hs_text_icu.h __hs_uspoof_openFromSource" uspoof_openFromSource
    :: CString -> Int32 -> CString -> Int32 -> Ptr Int32 -> Ptr Int32 -> Ptr UErrorCode -> IO (Ptr USpoof)

foreign import ccall unsafe "hs_text_icu.h __hs_uspoof_getChecks" uspoof_getChecks
    :: Ptr USpoof -> Ptr UErrorCode -> IO USpoofCheck

foreign import ccall unsafe "hs_text_icu.h __hs_uspoof_setChecks" uspoof_setChecks
    :: Ptr USpoof -> USpoofCheck -> Ptr UErrorCode -> IO ()

foreign import ccall unsafe "hs_text_icu.h __hs_uspoof_getRestrictionLevel" uspoof_getRestrictionLevel
    :: Ptr USpoof -> IO URestrictionLevel

foreign import ccall unsafe "hs_text_icu.h __hs_uspoof_setRestrictionLevel" uspoof_setRestrictionLevel
    :: Ptr USpoof -> URestrictionLevel -> IO ()

foreign import ccall unsafe "hs_text_icu.h __hs_uspoof_getAllowedLocales" uspoof_getAllowedLocales
    :: Ptr USpoof -> Ptr UErrorCode -> IO CString

foreign import ccall unsafe "hs_text_icu.h __hs_uspoof_setAllowedLocales" uspoof_setAllowedLocales
    :: Ptr USpoof -> CString -> Ptr UErrorCode -> IO ()

foreign import ccall unsafe "hs_text_icu.h __hs_uspoof_areConfusable" uspoof_areConfusable
    :: Ptr USpoof -> Ptr UChar -> Int32 -> Ptr UChar -> Int32 -> Ptr UErrorCode -> IO USpoofCheck

foreign import ccall unsafe "hs_text_icu.h __hs_uspoof_check" uspoof_check
    :: Ptr USpoof -> Ptr UChar -> Int32 -> Ptr Int32 -> Ptr UErrorCode -> IO USpoofCheck

foreign import ccall unsafe "hs_text_icu.h __hs_uspoof_getSkeleton" uspoof_getSkeleton
    :: Ptr USpoof -> USkeletonTypeOverride -> Ptr UChar -> Int32 -> Ptr UChar -> Int32 -> Ptr UErrorCode -> IO Int32

foreign import ccall unsafe "hs_text_icu.h __hs_uspoof_serialize" uspoof_serialize
    :: Ptr USpoof -> Ptr Word8 -> Int32 -> Ptr UErrorCode -> IO Int32

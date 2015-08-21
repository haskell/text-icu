{-# LANGUAGE BangPatterns, CPP, DeriveDataTypeable, ForeignFunctionInterface #-}
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
    -- * Functions
    , open
    , getSkeleton
    , getChecks
    , setChecks
    , getRestrictionLevel
    , setRestrictionLevel
    , areConfusable
    , spoofCheck
    ) where

#include <unicode/uspoof.h>

import Control.Applicative
import Data.Bits ((.&.))
import Data.Int (Int32)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text.Foreign (fromPtr, useAsPtr)
import Data.Text.ICU.BitMask (ToBitMask, fromBitMask, toBitMask)
import Data.Text.ICU.Spoof.Internal (MSpoof, USpoof, Spoof, withSpoof, wrap)
import Data.Text.ICU.Error.Internal (UErrorCode, handleError)
import Data.Text.ICU.Internal (UChar)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr, nullPtr)

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
open = wrap =<< (handleError uspoof_open)

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
getSkeleton :: MSpoof -> [SpoofCheck] -> Text -> IO Text
getSkeleton s c t = do
  withSpoof s $ \sptr ->
    useAsPtr t $ \tptr tlen -> do
      allocaArray (fromIntegral tlen) $ \destptr ->
        (fromPtr destptr . fromIntegral) =<<
          handleError (uspoof_getSkeleton sptr (fromIntegral $ toBitMask c) tptr (fromIntegral tlen) destptr (fromIntegral tlen))

-- | Check if a string could be confused with any other.
spoofCheck :: MSpoof -> Text -> IO SpoofCheckResult
spoofCheck s t = do
  withSpoof s $ \sptr ->
    useAsPtr t $ \tptr tlen ->
      makeSpoofCheckResult <$> handleError (uspoof_check sptr tptr (fromIntegral tlen) nullPtr)

foreign import ccall unsafe "hs_text_icu.h __hs_uspoof_open" uspoof_open
    :: Ptr UErrorCode -> IO (Ptr USpoof)

foreign import ccall unsafe "hs_text_icu.h __hs_uspoof_getChecks" uspoof_getChecks
    :: Ptr USpoof -> Ptr UErrorCode -> IO USpoofCheck

foreign import ccall unsafe "hs_text_icu.h __hs_uspoof_setChecks" uspoof_setChecks
    :: Ptr USpoof -> USpoofCheck -> Ptr UErrorCode -> IO ()

foreign import ccall unsafe "hs_text_icu.h __hs_uspoof_getRestrictionLevel" uspoof_getRestrictionLevel
    :: Ptr USpoof -> IO URestrictionLevel

foreign import ccall unsafe "hs_text_icu.h __hs_uspoof_setRestrictionLevel" uspoof_setRestrictionLevel
    :: Ptr USpoof -> URestrictionLevel -> IO ()

foreign import ccall unsafe "hs_text_icu.h __hs_uspoof_areConfusable" uspoof_areConfusable
    :: Ptr USpoof -> Ptr UChar -> Int32 -> Ptr UChar -> Int32 -> Ptr UErrorCode -> IO USpoofCheck

foreign import ccall unsafe "hs_text_icu.h __hs_uspoof_check" uspoof_check
    :: Ptr USpoof -> Ptr UChar -> Int32 -> Ptr Int32 -> Ptr UErrorCode -> IO USpoofCheck

foreign import ccall unsafe "hs_text_icu.h __hs_uspoof_getSkeleton" uspoof_getSkeleton
    :: Ptr USpoof -> USpoofCheck -> Ptr UChar -> Int32 -> Ptr UChar -> Int32 -> Ptr UErrorCode -> IO Int32

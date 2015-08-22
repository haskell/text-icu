{-# LANGUAGE BangPatterns, CPP, DeriveDataTypeable, ForeignFunctionInterface,
    OverloadedStrings #-}
-- |
-- Module      : Data.Text.ICU.Spoof
-- Copyright   : (c) 2015 Ben Hamilton
--
-- License     : BSD-style
-- Maintainer  : bgertzfield@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- String spoofing (confusability) checks for Unicode, implemented as
-- bindings to the International Components for Unicode (ICU) uspoof
-- library.
--
-- See <http://unicode.org/reports/tr36/ UTR #36> and
-- <http://unicode.org/reports/tr39/ UTS #39> for detailed information
-- about the underlying algorithms and databases used by this module.

module Data.Text.ICU.Spoof
    (
    -- * Unicode spoof checking API
    -- $api
    -- * Types
      MSpoof
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
import Data.Text (Text, pack, splitOn, strip, unpack)
import Data.Text.Foreign (fromPtr, useAsPtr)
import Data.Text.ICU.BitMask (ToBitMask, fromBitMask, highestValueInBitMask,
                              toBitMask)
import Data.Text.ICU.Spoof.Internal (MSpoof, USpoof, withSpoof, wrap,
                                     wrapWithSerialized)
import Data.Text.ICU.Error.Internal (UErrorCode, handleError,
                                     handleOverflowError)
import Data.Text.ICU.Internal (UChar)
import Data.Word (Word8)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.ForeignPtr (withForeignPtr)

-- $api
--
-- The 'spoofCheck', 'areConfusable', and 'getSkeleton' functions analyze
-- Unicode text for visually confusable (or \"spoof\") characters.
--
-- For example, Latin, Cyrillic, and Greek all contain unique Unicode
-- values which appear nearly identical on-screen:
--
-- @
--      A    0041    LATIN CAPITAL LETTER A
--      &#x0391;    0391    GREEK CAPITAL LETTER ALPHA
--      &#x0410;    0410    CYRILLIC CAPITAL LETTER A
--      &#x13AA;    13AA    CHEROKEE LETTER GO
--      &#x1D00;    1D00    LATIN LETTER SMALL CAPITAL A
--      &#x15C5;    15C5    CANADIAN SYLLABICS CARRIER GHO
--      &#xFF21;    FF21    FULLWIDTH LATIN CAPITAL LETTER A
--      &#x102A0;    102A0   CARIAN LETTER A
--      &#x1D400;    1D400   MATHEMATICAL BOLD CAPITAL A
-- @
--
-- and so on. To check a string for visually confusable characters:
--
--   1. 'open' an 'MSpoof'
--   2. optionally configure it with 'setChecks', 'setRestrictionLevel',
-- and/or 'setAllowedLocales', then
--   3. 'spoofCheck' a single string, use 'areConfusable' to check if two
-- strings could be confused for each other, or use 'getSkeleton' to precompute
-- a \"skeleton\" string (similar to a hash code) which can be cached
-- and re-used to quickly check (using Unicode string comparison) if
-- two strings are confusable.
--
-- By default, these methods will use ICU's bundled copy of
-- <http://unicode.org/Public/security/latest/confusables.txt confusables.txt>
-- and <http://unicode.org/Public/security/latest/confusablesWholeScript.txt confusablesWholeScript.txt>,
-- which could be out of date. To provide your own confusables databases, use
-- 'openFromSource'. (To avoid repeatedly parsing these databases, you
-- can then 'serialize' your configured 'MSpoof' and later
-- 'openFromSerialized' to load the pre-parsed databases.)

data SpoofCheck
  -- | Makes 'areConfusable' report if both identifiers are both from the
  -- same script and are visually confusable. Does not affect 'spoofCheck'.
  = SingleScriptConfusable

  -- | Makes 'areConfusable' report if both identifiers are visually
  -- confusable and at least one identifier contains characters from more
  -- than one script.
  --
  -- Makes 'spoofCheck' report if the identifier contains multiple scripts,
  -- and is confusable with some other identifier in a single script.
  | MixedScriptConfusable

  -- | Makes 'areConfusable' report if each identifier is of a different
  -- single script, and the identifiers are visually confusable.
  | WholeScriptConfusable

  -- | By default, spoof checks assume the strings have been processed
  -- through 'toCaseFold' and only check lower-case identifiers. If
  -- this is set, spoof checks will check both upper and lower case
  -- identifiers.
  | AnyCase

  -- | Checks that identifiers are no looser than the specified
  -- level passed to 'setRestrictionLevel'.
  | RestrictionLevel

  -- | Checks the identifier for the presence of invisible characters,
  -- such as zero-width spaces, or character sequences that are likely
  -- not to display, such as multiple occurrences of the same
  -- non-spacing mark.
  | Invisible

  -- | Checks whether the identifier contains only characters from a
  -- specified set (for example, via 'setAllowedLocales').
  | CharLimit

  -- | Checks that the identifier contains numbers from only a
  -- single script.
  | MixedNumbers

  -- | Enables all checks.
  | AllChecks

  -- | Enables returning a 'RestrictionLevel' in the 'SpoofCheckResult'.
  | AuxInfo
  deriving (Bounded, Enum, Eq, Show)

instance ToBitMask SpoofCheck where
  toBitMask SingleScriptConfusable = #const USPOOF_SINGLE_SCRIPT_CONFUSABLE
  toBitMask MixedScriptConfusable  = #const USPOOF_MIXED_SCRIPT_CONFUSABLE
  toBitMask WholeScriptConfusable  = #const USPOOF_WHOLE_SCRIPT_CONFUSABLE
  toBitMask AnyCase                = #const USPOOF_ANY_CASE
  toBitMask RestrictionLevel       = #const USPOOF_RESTRICTION_LEVEL
  toBitMask Invisible              = #const USPOOF_INVISIBLE
  toBitMask CharLimit              = #const USPOOF_CHAR_LIMIT
  toBitMask MixedNumbers           = #const USPOOF_MIXED_NUMBERS
  toBitMask AllChecks              = #const USPOOF_ALL_CHECKS
  toBitMask AuxInfo                = #const USPOOF_AUX_INFO

type USpoofCheck = Int32

data RestrictionLevel
  -- | Checks that the string contains only Unicode values in the range
  -- #0000#&#2013;#007F# inclusive.
  = ASCII
  -- | Checks that the string contains only characters from a single script.
  | SingleScriptRestrictive
  -- | Checks that the string contains only characters from a single script,
  -- or from the combinations (Latin + Han + Hiragana + Katakana),
  -- (Latin + Han + Bopomofo), or (Latin + Han + Hangul).
  | HighlyRestrictive
  -- | Checks that the string contains only characters from the combinations
  -- (Latin + Cyrillic + Greek + Cherokee), (Latin + Han + Hiragana + Katakana),
  -- (Latin + Han + Bopomofo), or (Latin + Han + Hangul).
  | ModeratelyRestrictive
  -- | Allows arbitrary mixtures of scripts.
  | MinimallyRestrictive
  -- | Allows any valid identifiers, including characters outside of the
  -- Identifier Profile.
  | Unrestrictive
  deriving (Bounded, Enum, Eq, Show)

instance ToBitMask RestrictionLevel where
  toBitMask ASCII                   = #const USPOOF_ASCII
  toBitMask SingleScriptRestrictive = #const USPOOF_SINGLE_SCRIPT_RESTRICTIVE
  toBitMask HighlyRestrictive       = #const USPOOF_HIGHLY_RESTRICTIVE
  toBitMask ModeratelyRestrictive   = #const USPOOF_MODERATELY_RESTRICTIVE
  toBitMask MinimallyRestrictive    = #const USPOOF_MINIMALLY_RESTRICTIVE
  toBitMask Unrestrictive           = #const USPOOF_UNRESTRICTIVE

type URestrictionLevel = Int32

data SpoofCheckResult
  -- | The string passed all configured spoof checks.
  = CheckOK
  -- | The string failed one or more spoof checks.
  | CheckFailed [SpoofCheck]
  -- | The string failed one or more spoof checks, and
  -- failed to pass the configured restriction level.
  | CheckFailedWithRestrictionLevel {
    -- | The spoof checks which the string failed.
    failedChecks :: [SpoofCheck]
    -- | The restriction level which the string failed to pass.
    , failedLevel :: RestrictionLevel
    }
  deriving (Eq, Show)

data SkeletonTypeOverride
  -- | By default, 'getSkeleton' builds skeletons which catch
  -- visually confusable characters across multiple scripts.
  -- Pass this flag to override that behavior and build skeletons
  -- which catch visually confusable characters across single scripts.
  = SkeletonSingleScript
  -- | By default, 'getSkeleton' assumes the input string has already
  -- been passed through 'toCaseFold' and is lower-case. Pass this
  -- flag to override that behavior and allow upper and lower-case strings.
  | SkeletonAnyCase
  deriving (Bounded, Enum, Eq, Show)

instance ToBitMask SkeletonTypeOverride where
  toBitMask SkeletonSingleScript = #const USPOOF_SINGLE_SCRIPT_CONFUSABLE
  toBitMask SkeletonAnyCase      = #const USPOOF_ANY_CASE

type USkeletonTypeOverride = Int32

makeSpoofCheckResult :: USpoofCheck -> SpoofCheckResult
makeSpoofCheckResult c =
  case c of
    0 -> CheckOK
    _ ->
      case restrictionLevel of
        Nothing -> CheckFailed spoofChecks
        Just l -> CheckFailedWithRestrictionLevel spoofChecks l
      where spoofChecks = fromBitMask $ fromIntegral $
                          c .&. #const USPOOF_ALL_CHECKS
            restrictionValue = c .&. #const USPOOF_RESTRICTION_LEVEL_MASK
            restrictionLevel = highestValueInBitMask $ fromIntegral $
                               restrictionValue

-- | Open a spoof checker for checking Unicode strings for lookalike
-- security issues with default options (all 'SpoofCheck's except
-- 'CharLimit').
open :: IO MSpoof
open = wrap =<< handleError uspoof_open

-- | Open a spoof checker with custom rules given the UTF-8 encoded
-- contents of the @confusables.txt@ and @confusablesWholeScript.txt@
-- files as described in <http://unicode.org/reports/tr39/ Unicode UAX #39>.
openFromSource :: (ByteString, ByteString) -> IO MSpoof
openFromSource (confusables, confusablesWholeScript) =
  unsafeUseAsCStringLen confusables $ \(cptr, clen) ->
    unsafeUseAsCStringLen confusablesWholeScript $ \(wptr, wlen) ->
      wrap =<< handleError (uspoof_openFromSource cptr (fromIntegral clen) wptr
                            (fromIntegral wlen) nullPtr nullPtr)

-- | Open a spoof checker previously serialized to bytes using 'serialize'.
-- The returned 'MSpoof' will retain a reference to the 'ForeignPtr' inside
-- the ByteString, so ensure its contents do not change for the lifetime
-- of the lifetime of the returned value.
openFromSerialized :: ByteString -> IO MSpoof
openFromSerialized b =
  case toForeignPtr b of
    (ptr, off, len) -> withForeignPtr ptr $ \p ->
      wrapWithSerialized ptr =<< handleError
      (uspoof_openFromSerialized (p `plusPtr` off) (fromIntegral len) nullPtr)

-- | Get the checks performed by a spoof checker.
getChecks :: MSpoof -> IO [SpoofCheck]
getChecks s = withSpoof s $ \sptr ->
  (fromBitMask . fromIntegral . (.&.) #{const USPOOF_ALL_CHECKS}) <$>
  handleError (uspoof_getChecks sptr)

-- | Configure the checks performed by a spoof checker.
setChecks :: MSpoof -> [SpoofCheck] -> IO ()
setChecks s c = withSpoof s $ \sptr ->
  handleError $ uspoof_setChecks sptr . fromIntegral $ toBitMask c

-- | Get the restriction level of a spoof checker.
getRestrictionLevel :: MSpoof -> IO (Maybe RestrictionLevel)
getRestrictionLevel s = withSpoof s $ \sptr ->
  (highestValueInBitMask . fromIntegral) <$> uspoof_getRestrictionLevel sptr

-- | Configure the restriction level of a spoof checker.
setRestrictionLevel :: MSpoof -> RestrictionLevel -> IO ()
setRestrictionLevel s l = withSpoof s $ \sptr ->
    uspoof_setRestrictionLevel sptr . fromIntegral $ toBitMask l

-- | Get the list of locale names allowed to be used with a spoof checker.
-- (We don't use 'LocaleName' since the root and default locales have no
-- meaning here.)
getAllowedLocales :: MSpoof -> IO [String]
getAllowedLocales s = withSpoof s $ \sptr ->
  splitLocales <$> (peekCString =<< handleError (uspoof_getAllowedLocales sptr))
  where splitLocales = fmap (unpack . strip) . splitOn "," . pack

-- | Get the list of locale names allowed to be used with a spoof checker.
-- (We don't use 'LocaleName' since the root and default locales have no
-- meaning here.)
setAllowedLocales :: MSpoof -> [String] -> IO ()
setAllowedLocales s locs = withSpoof s $ \sptr ->
  withCString (intercalate "," locs) $ \lptr ->
    handleError (uspoof_setAllowedLocales sptr lptr)

-- | Check if two strings could be confused with each other.
areConfusable :: MSpoof -> Text -> Text -> IO SpoofCheckResult
areConfusable s t1 t2 = withSpoof s $ \sptr ->
  useAsPtr t1 $ \t1ptr t1len ->
    useAsPtr t2 $ \t2ptr t2len ->
      makeSpoofCheckResult <$>
      handleError (uspoof_areConfusable sptr
                   t1ptr (fromIntegral t1len)
                   t2ptr (fromIntegral t2len))

-- | Generates re-usable "skeleton" strings which can be used (via
-- Unicode equality) to check if an identifier is confusable
-- with some large set of existing identifiers.
--
-- If you cache the returned strings in storage, you /must/ invalidate
-- your cache any time the underlying confusables database changes
-- (i.e., on ICU upgrade).
--
-- By default, assumes all input strings have been passed through
-- 'toCaseFold' and are lower-case. To change this, pass
-- 'SkeletonAnyCase'.
--
-- By default, builds skeletons which catch visually confusable
-- characters across multiple scripts.  Pass 'SkeletonSingleScript' to
-- override that behavior and build skeletons which catch visually
-- confusable characters across single scripts.
getSkeleton :: MSpoof -> Maybe SkeletonTypeOverride -> Text -> IO Text
getSkeleton s o t = withSpoof s $ \sptr ->
  useAsPtr t $ \tptr tlen ->
    handleOverflowError (fromIntegral tlen)
      (\dptr dlen -> uspoof_getSkeleton sptr oflags tptr
                     (fromIntegral tlen) dptr (fromIntegral dlen))
      (\dptr dlen -> fromPtr (castPtr dptr) (fromIntegral dlen))
    where oflags = maybe 0 (fromIntegral . toBitMask) o

-- | Checks if a string could be confused with any other.
spoofCheck :: MSpoof -> Text -> IO SpoofCheckResult
spoofCheck s t = withSpoof s $ \sptr ->
  useAsPtr t $ \tptr tlen ->
    makeSpoofCheckResult <$> handleError
      (uspoof_check sptr tptr (fromIntegral tlen) nullPtr)

-- | Serializes the rules in this spoof checker to a byte array,
-- suitable for re-use by 'openFromSerialized'.
--
-- Only includes any data provided to 'openFromSource'. Does not
-- include any other state or configuration.
serialize :: MSpoof -> IO ByteString
serialize s = withSpoof s $ \sptr ->
  handleOverflowError 0
    (\dptr dlen -> (uspoof_serialize sptr dptr (fromIntegral dlen)))
    (\dptr dlen -> create (fromIntegral dlen) $ \bptr ->
      memcpy dptr bptr (fromIntegral dlen))

foreign import ccall unsafe "hs_text_icu.h __hs_uspoof_open" uspoof_open
    :: Ptr UErrorCode -> IO (Ptr USpoof)

foreign import ccall unsafe "hs_text_icu.h __hs_uspoof_openFromSerialized"
  uspoof_openFromSerialized
    :: Ptr Word8 -> Int32 -> Ptr Int32 -> Ptr UErrorCode -> IO (Ptr USpoof)

foreign import ccall unsafe "hs_text_icu.h __hs_uspoof_openFromSource"
  uspoof_openFromSource
    :: CString -> Int32 -> CString -> Int32 -> Ptr Int32 -> Ptr Int32 ->
       Ptr UErrorCode -> IO (Ptr USpoof)

foreign import ccall unsafe "hs_text_icu.h __hs_uspoof_getChecks"
  uspoof_getChecks
    :: Ptr USpoof -> Ptr UErrorCode -> IO USpoofCheck

foreign import ccall unsafe "hs_text_icu.h __hs_uspoof_setChecks"
  uspoof_setChecks
    :: Ptr USpoof -> USpoofCheck -> Ptr UErrorCode -> IO ()

foreign import ccall unsafe "hs_text_icu.h __hs_uspoof_getRestrictionLevel"
  uspoof_getRestrictionLevel
    :: Ptr USpoof -> IO URestrictionLevel

foreign import ccall unsafe "hs_text_icu.h __hs_uspoof_setRestrictionLevel"
  uspoof_setRestrictionLevel
    :: Ptr USpoof -> URestrictionLevel -> IO ()

foreign import ccall unsafe "hs_text_icu.h __hs_uspoof_getAllowedLocales"
  uspoof_getAllowedLocales
    :: Ptr USpoof -> Ptr UErrorCode -> IO CString

foreign import ccall unsafe "hs_text_icu.h __hs_uspoof_setAllowedLocales"
  uspoof_setAllowedLocales
    :: Ptr USpoof -> CString -> Ptr UErrorCode -> IO ()

foreign import ccall unsafe "hs_text_icu.h __hs_uspoof_areConfusable"
  uspoof_areConfusable
    :: Ptr USpoof -> Ptr UChar -> Int32 -> Ptr UChar -> Int32 -> Ptr UErrorCode
       -> IO USpoofCheck

foreign import ccall unsafe "hs_text_icu.h __hs_uspoof_check" uspoof_check
    :: Ptr USpoof -> Ptr UChar -> Int32 -> Ptr Int32 -> Ptr UErrorCode
       -> IO USpoofCheck

foreign import ccall unsafe "hs_text_icu.h __hs_uspoof_getSkeleton"
  uspoof_getSkeleton
    :: Ptr USpoof -> USkeletonTypeOverride -> Ptr UChar -> Int32 -> Ptr UChar ->
       Int32 -> Ptr UErrorCode -> IO Int32

foreign import ccall unsafe "hs_text_icu.h __hs_uspoof_serialize"
  uspoof_serialize
    :: Ptr USpoof -> Ptr Word8 -> Int32 -> Ptr UErrorCode -> IO Int32

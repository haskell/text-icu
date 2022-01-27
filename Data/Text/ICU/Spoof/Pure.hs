-- |
-- Module      : Data.Text.ICU.Spoof.Pure
-- Copyright   : (c) 2015 Ben Hamilton
--
-- License     : BSD-style
-- Maintainer  : bgertzfield@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Pure string spoof checking functions for Unicode, implemented as
-- bindings to the International Components for Unicode (ICU)
-- libraries.
--
-- For the impure spoof checking API (which is richer, but less easy to
-- use), see the "Data.Text.ICU.Spoof" module.

module Data.Text.ICU.Spoof.Pure
    (
    -- * Types
      Spoof
    , SpoofParams(..)
    , spoof
    , spoofWithParams
    , spoofFromSource
    , spoofFromSerialized
    -- * String spoof checks
    , areConfusable
    , getSkeleton
    , spoofCheck
    -- * Configuration
    , getAllowedLocales
    , getChecks
    , getRestrictionLevel
    -- * Persistence
    , serialize
    ) where

import Data.ByteString (ByteString)
import Data.Foldable (forM_)
import Data.Text (Text)
import Data.Text.ICU.Spoof.Internal (Spoof(..))
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text.ICU.Spoof as S

data SpoofParams
  -- | Used to configure a 'Spoof' checker via 'spoofWithParams'.
  = SpoofParams {
    -- | Optional 'S.SpoofCheck's to perform on a string. By default, performs
    -- all checks except 'CharLimit'.
    spoofChecks :: Maybe [S.SpoofCheck]
    -- | Optional 'S.RestrictionLevel' to which characters in the string will
    -- be limited. By default, uses 'HighlyRestrictive'.
  , level :: Maybe S.RestrictionLevel
    -- | Optional locale(s) whose scripts will be used to limit the
    -- set of allowed characters in a string. If set, automatically
    -- enables the 'CharLimit' spoof check.
  , locales :: Maybe [String]
} deriving (Show, Eq)

applySpoofParams :: SpoofParams -> S.MSpoof -> S.MSpoof
applySpoofParams (SpoofParams c lev loc) s = unsafePerformIO $ do
  forM_ c (S.setChecks s)
  forM_ lev (S.setRestrictionLevel s)
  forM_ loc (S.setAllowedLocales s)
  return s

-- | Open an immutable 'Spoof' checker with default options (all
-- 'S.SpoofCheck's except 'CharLimit').
spoof :: Spoof
spoof = unsafePerformIO $ S `fmap` S.open
{-# NOINLINE spoof #-}

-- | Open an immutable 'Spoof' checker with specific 'SpoofParams'
-- to control its behavior.
spoofWithParams :: SpoofParams -> Spoof
spoofWithParams p = unsafePerformIO $ do
  s <- S.open
  return (S $ applySpoofParams p s)

-- | Open a immutable 'Spoof' checker with specific 'SpoofParams'
-- to control its behavior and custom rules given the UTF-8 encoded
-- contents of the @confusables.txt@ and @confusablesWholeScript.txt@
-- files as described in <http://unicode.org/reports/tr39/ Unicode UAX #39>.
spoofFromSource :: (ByteString, ByteString) -> SpoofParams -> Spoof
spoofFromSource (confusables, confusablesWholeScript) p = unsafePerformIO $ do
  s <- S.openFromSource (confusables, confusablesWholeScript)
  return (S $ applySpoofParams p s)

-- | Create an immutable spoof checker with specific 'SpoofParams'
-- to control its behavior and custom rules previously returned
-- by 'serialize'.
spoofFromSerialized :: ByteString -> SpoofParams -> Spoof
spoofFromSerialized b p = unsafePerformIO $ do
  s <- S.openFromSerialized b
  return (S $ applySpoofParams p s)

-- | Check two strings for confusability.
areConfusable :: Spoof -> Text -> Text -> S.SpoofCheckResult
areConfusable (S s) t1 t2 = unsafePerformIO $ S.areConfusable s t1 t2

-- | Check a string for spoofing issues.
spoofCheck :: Spoof -> Text -> S.SpoofCheckResult
spoofCheck (S s) t = unsafePerformIO $ S.spoofCheck s t

-- | Generates re-usable \"skeleton\" strings which can be used (via
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
getSkeleton :: Spoof -> Maybe S.SkeletonTypeOverride -> Text -> Text
getSkeleton (S s) o t = unsafePerformIO $ S.getSkeleton s o t

-- | Gets the restriction level currently configured in the spoof
-- checker, if present.
getRestrictionLevel :: Spoof -> Maybe S.RestrictionLevel
getRestrictionLevel (S s) = unsafePerformIO $ S.getRestrictionLevel s

-- | Gets the checks currently configured in the spoof checker.
getChecks :: Spoof -> [S.SpoofCheck]
getChecks (S s) = unsafePerformIO $ S.getChecks s

-- | Gets the locales whose scripts are currently allowed by the spoof
-- checker.  (We don't use 'LocaleName' since the root and default
-- locales have no meaning here.)
getAllowedLocales :: Spoof -> [String]
getAllowedLocales (S s) = unsafePerformIO $ S.getAllowedLocales s

-- | Serializes the rules in this spoof checker to a byte array,
-- suitable for re-use by 'spoofFromSerialized'.
--
-- Only includes any data provided to 'openFromSource'. Does not
-- include any other state or configuration.
serialize :: Spoof -> ByteString
serialize (S s) = unsafePerformIO $ S.serialize s

{-# INLINE spoofCheck #-}

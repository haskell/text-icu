{-# LANGUAGE DeriveDataTypeable, ForeignFunctionInterface #-}
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
    -- * Unicode spoof checking API
    -- $api
      Spoof
    , SpoofParams(..)
    , spoof
    , spoofWithParams
    , spoofFromSerialized
    , areConfusable
    , getAllowedLocales
    , getChecks
    , getSkeleton
    , getRestrictionLevel
    , serialize
    , spoofCheck
    ) where

import Data.ByteString (ByteString)
import Data.Foldable (forM_)
import Data.Text (Text)
import Data.Text.ICU.Spoof.Internal (Spoof(..))
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text.ICU.Spoof as S

data SpoofParams = SpoofParams {
     spoofChecks :: Maybe [S.SpoofCheck]
   , level :: Maybe S.RestrictionLevel
   , locales :: Maybe [String]
} deriving (Show, Eq)

-- $api
--

-- | Create an immutable spoof checker with default options.
spoof :: Spoof
spoof = unsafePerformIO $ C `fmap` S.open

-- | Create an immutable spoof checker with specific options.
spoofWithParams :: SpoofParams -> Spoof
spoofWithParams (SpoofParams c lev loc) = unsafePerformIO $ do
  s <- S.open
  forM_ c (S.setChecks s)
  forM_ lev (S.setRestrictionLevel s)
  forM_ loc (S.setAllowedLocales s)
  return (C s)

-- | Create an immutable spoof checker from a previously-serialized instance.
spoofFromSerialized :: ByteString -> Spoof
spoofFromSerialized b = unsafePerformIO $ C `fmap` S.openFromSerialized b

-- | Check two strings for confusability.
areConfusable :: Spoof -> Text -> Text -> S.SpoofCheckResult
areConfusable (C s) t1 t2 = unsafePerformIO $ S.areConfusable s t1 t2

-- | Check a string for spoofing issues.
spoofCheck :: Spoof -> Text -> S.SpoofCheckResult
spoofCheck (C s) t = unsafePerformIO $ S.spoofCheck s t

-- | Get a skeleton representation of a string to directly compare for
-- spoofability with another string.
getSkeleton :: Spoof -> Maybe S.SkeletonTypeOverride -> Text -> Text
getSkeleton (C s) o t = unsafePerformIO $ S.getSkeleton s o t

-- | Gets the restriction level currently configured in the spoof
-- checker, if present.
getRestrictionLevel :: Spoof -> Maybe S.RestrictionLevel
getRestrictionLevel (C s) = unsafePerformIO $ S.getRestrictionLevel s

-- | Gets the checks currently configured in the spoof checker.
getChecks :: Spoof -> [S.SpoofCheck]
getChecks (C s) = unsafePerformIO $ S.getChecks s

-- | Gets the locales whose scripts are currently allowed by the spoof
-- checker.  (We don't use LocaleName since the root and default
-- locales have no meaning here.)
getAllowedLocales :: Spoof -> [String]
getAllowedLocales (C s) = unsafePerformIO $ S.getAllowedLocales s

-- | Serializes the configured spoof checker so it can later be
-- created with openFromSerialized.
serialize :: Spoof -> ByteString
serialize (C s) = unsafePerformIO $ S.serialize s

{-# INLINE spoofCheck #-}

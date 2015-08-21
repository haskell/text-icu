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
    , spoof
    , spoofWithChecks
    , spoofWithLevel
    , spoofWithChecksAndLevel
    , areConfusable
    , getSkeleton
    , spoofCheck
    ) where

import Data.Text (Text)
import Data.Text.ICU.Spoof.Internal (Spoof(..))
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text.ICU.Spoof as S

-- $api
--

-- | Create an immutable spoof checker with default options.
spoof :: Spoof
spoof = unsafePerformIO $ C `fmap` S.open

-- | Create an immutable spoof checker with specific options.
spoofWithChecks :: [S.SpoofCheck] -> Spoof
spoofWithChecks checks = unsafePerformIO $ do
  s <- S.open
  S.setChecks s checks
  return (C s)

-- | Create an immutable spoof checker with specific options and restriction level.
spoofWithLevel :: S.RestrictionLevel -> Spoof
spoofWithLevel level = unsafePerformIO $ do
  s <- S.open
  S.setRestrictionLevel s level
  return (C s)

spoofWithChecksAndLevel :: [S.SpoofCheck] -> S.RestrictionLevel -> Spoof
spoofWithChecksAndLevel checks level = unsafePerformIO $ do
  s <- S.open
  S.setChecks s checks
  S.setRestrictionLevel s level
  return (C s)

-- | Check two strings for confusability.
areConfusable :: Spoof -> Text -> Text -> S.SpoofCheckResult
areConfusable (C s) t1 t2 = unsafePerformIO $ S.areConfusable s t1 t2

-- | Check a string for spoofing issues.
spoofCheck :: Spoof -> Text -> S.SpoofCheckResult
spoofCheck (C s) t = unsafePerformIO $ S.spoofCheck s t

-- | Get a skeleton representation of a string to directly compare for
-- spoofability with another string.
getSkeleton :: Spoof -> [S.SpoofCheck] -> Text -> Text
getSkeleton (C s) c t = unsafePerformIO $ S.getSkeleton s c t

{-# INLINE spoofCheck #-}

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
    , areConfusable
    , getSkeleton
    , spoofCheck
    ) where

import Data.Text (Text)
import Data.Text.ICU.Internal (LocaleName(..))
import Data.Text.ICU.Spoof.Internal (Spoof(..))
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text.ICU.Spoof as S

data SpoofParams = SpoofParams {
     spoofChecks :: Maybe [S.SpoofCheck]
   , level :: Maybe S.RestrictionLevel
   , locales :: Maybe [LocaleName]
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
  case c of
    Just c' -> S.setChecks s c'
    Nothing -> return ()
  case lev of
    Just lev' -> S.setRestrictionLevel s lev'
    Nothing -> return ()
  case loc of
    Just loc' -> S.setAllowedLocales s loc'
    Nothing -> return ()
  return (C s)

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

{-# INLINE spoofCheck #-}

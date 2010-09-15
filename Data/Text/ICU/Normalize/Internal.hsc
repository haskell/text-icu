{-# LANGUAGE CPP, DeriveDataTypeable, ForeignFunctionInterface #-}
-- |
-- Module      : Data.Text.ICU.Normalize.Internal
-- Copyright   : (c) 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC

module Data.Text.ICU.Normalize.Internal
    (
      UNormalizationCheckResult
    , toNCR
    ) where

#include <unicode/unorm.h>

import Foreign.C.Types (CInt)

type UNormalizationCheckResult = CInt

toNCR :: UNormalizationCheckResult -> Maybe Bool
toNCR (#const UNORM_NO)    = Just False
toNCR (#const UNORM_MAYBE) = Nothing
toNCR (#const UNORM_YES)   = Just True
toNCR _                    = error "toNormalizationCheckResult"

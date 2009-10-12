{-# LANGUAGE DeriveDataTypeable #-}

module Data.Text.ICU.Locale.Internal
    (
      Locale(..)
    ) where

import Data.Data (Data)
import Data.Typeable (Typeable)

newtype Locale = Locale {
      localeName :: String
    } deriving (Eq, Ord, Show, Typeable, Data)

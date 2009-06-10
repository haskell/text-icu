-- |This module imports the most important ICU modules and re-exports 
-- them. See the individual modules for further information.
module Data.Text.ICU (
  module Data.Text,
  module Data.Text.ICU.Calendar,
  module Data.Text.ICU.NumberFormat,
  module Data.Text.ICU.Normalizer,
  module Data.Text.ICU.DateTimeFormat,
  module Data.Text.ICU.Locale,
  module Data.Text.ICU.Error,
  module Data.Text.ICU.Format)
where

import Data.Text
import Data.Text.ICU.Calendar
import Data.Text.ICU.NumberFormat
import Data.Text.ICU.Normalizer
import Data.Text.ICU.DateTimeFormat
import Data.Text.ICU.Locale
import Data.Text.ICU.Error
import Data.Text.ICU.Formatting
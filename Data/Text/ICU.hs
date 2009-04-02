-- |This module imports the most important ICU modules and re-exports 
-- them. See the individual modules for further information.
module Data.Text.ICU (
  module Data.Text,
  module Data.Text.ICU.Calendars,
  module Data.Text.ICU.NumberFormatting,
  module Data.Text.ICU.Normalizer,
  module Data.Text.ICU.DateTimeFormatting,
  module Data.Text.ICU.Locales,
  module Data.Text.ICU.Error,
  module Data.Text.ICU.Formatting)
where

import Data.Text
import Data.Text.ICU.Calendars
import Data.Text.ICU.NumberFormatting
import Data.Text.ICU.Normalizer
import Data.Text.ICU.DateTimeFormatting
import Data.Text.ICU.Locales
import Data.Text.ICU.Error
import Data.Text.ICU.Formatting
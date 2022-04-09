{-# LANGUAGE CPP, ForeignFunctionInterface #-}

-- |
-- Module      : Data.Text.ICU.Shape
-- Copyright   : (c) 2018 Ondrej Palkovsky
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Bindings for arabic shaping, implemented as bindings to
-- the International Components for Unicode (ICU) libraries.
--

module Data.Text.ICU.Shape
  (
      shapeArabic
    , ShapeOption(..)
  ) where

#include <unicode/ushape.h>

import Data.List (foldl')
import Data.Text.ICU.Error.Internal (UErrorCode, handleOverflowError)
import Data.Bits ((.|.))
import Data.Int (Int32)
import Foreign.Ptr (Ptr)
import Data.Text.ICU.Internal (UChar, useAsUCharPtr, fromUCharPtr)
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)

-- | Options for the 'shapeArabic' function.
data ShapeOption =
    AggregateTaskheel
  -- ^ Tashkeel aggregation option: Replaces any combination of U+0651 with one of U+064C, U+064D, U+064E, U+064F, U+0650 with U+FC5E, U+FC5F, U+FC60, U+FC61, U+FC62 consecutively.
  | DigitTypeAnExtended
  -- ^ Digit type option: Use Eastern (Extended) Arabic-Indic digits (U+06f0...U+06f9).
  | DigitsAlen2AnInitAl
  -- ^ Replace European digits (U+0030...) by Arabic-Indic digits if the most recent strongly
  -- directional character is an Arabic letter (u_charDirection() result U_RIGHT_TO_LEFT_ARABIC [AL]).
  | DigitsAlen2AnInitLr
  -- ^ Digit shaping option: Replace European digits (U+0030...) by Arabic-Indic digits if the most recent strongly directional character is an Arabic letter (u_charDirection() result U_RIGHT_TO_LEFT_ARABIC [AL]).
  | DigitsAn2En
  -- ^ Digit shaping option: Replace Arabic-Indic digits by European digits (U+0030...).
  | DigitsEn2An
  -- ^ Digit shaping option: Replace European digits (U+0030...) by Arabic-Indic digits.
  | LengthFixedSpacesAtBeginning
  -- ^ If more room is necessary, then try to consume spaces at the beginning of the text.
  | LengthFixedSpacesAtEnd
  -- ^ If more room is necessary, then try to consume spaces at the end of the text.
  | LengthFixedSpacesNear
  -- ^ If more room is necessary, then try to consume spaces next to modified characters.
  | LettersShape
  -- ^ Letter shaping option: replace abstract letter characters by "shaped" ones.
  | LettersUnshape
  -- ^ Letter shaping option: replace "shaped" letter characters by abstract ones.
  | LettersShapeTashkeelIsolated
  -- ^ The only difference with LettersShape is that Tashkeel letters are always "shaped" into the isolated form instead of the medial form (selecting codepoints from the Arabic Presentation Forms-B block).
  | PreservePresentation
  -- ^ Presentation form option: Don't replace Arabic Presentation Forms-A and Arabic Presentation Forms-B characters with 0+06xx characters, before shaping.
  | TextDirectionVisualLTR
  -- ^ Direction indicator: the source is in visual LTR order, the leftmost displayed character stored first.
  deriving (Show)

reduceShapeOpts :: [ShapeOption] -> Int32
reduceShapeOpts = foldl' orO 0
    where a `orO` b = a .|. fromShapeOption b

fromShapeOption :: ShapeOption -> Int32
fromShapeOption AggregateTaskheel   = #const U_SHAPE_AGGREGATE_TASHKEEL
fromShapeOption DigitTypeAnExtended   = #const U_SHAPE_DIGIT_TYPE_AN_EXTENDED
fromShapeOption DigitsAlen2AnInitAl = #const U_SHAPE_DIGITS_ALEN2AN_INIT_AL
fromShapeOption DigitsAlen2AnInitLr = #const U_SHAPE_DIGITS_ALEN2AN_INIT_LR
fromShapeOption DigitsAn2En = #const U_SHAPE_DIGITS_AN2EN
fromShapeOption DigitsEn2An = #const U_SHAPE_DIGITS_EN2AN
fromShapeOption LengthFixedSpacesAtBeginning = #const U_SHAPE_LENGTH_FIXED_SPACES_AT_BEGINNING
fromShapeOption LengthFixedSpacesAtEnd = #const U_SHAPE_LENGTH_FIXED_SPACES_AT_END
fromShapeOption LengthFixedSpacesNear = #const U_SHAPE_LENGTH_FIXED_SPACES_NEAR
fromShapeOption LettersShape = #const U_SHAPE_LETTERS_SHAPE
fromShapeOption LettersUnshape = #const U_SHAPE_LETTERS_UNSHAPE
fromShapeOption LettersShapeTashkeelIsolated = #const U_SHAPE_LETTERS_SHAPE_TASHKEEL_ISOLATED
fromShapeOption PreservePresentation = #const U_SHAPE_PRESERVE_PRESENTATION
fromShapeOption TextDirectionVisualLTR = #const U_SHAPE_TEXT_DIRECTION_VISUAL_LTR

-- | Shape Arabic text on a character basis.
--
-- Text-based shaping means that some character codepoints in the text are replaced by
-- others depending on the context. It transforms one kind of text into another.
-- In comparison, modern displays for Arabic text select appropriate, context-dependent font
-- glyphs for each text element, which means that they transform text into a glyph vector.
--
-- You probably want to call this with the LettersShape option in the default case.
shapeArabic :: [ShapeOption] -> Text -> Text
shapeArabic options t = unsafePerformIO . useAsUCharPtr t $ \sptr slen ->
  let slen' = fromIntegral slen
      options' = reduceShapeOpts options
  in handleOverflowError (fromIntegral slen)
      (\dptr dlen -> u_shapeArabic sptr slen' dptr (fromIntegral dlen) options')
      (\dptr dlen -> fromUCharPtr dptr (fromIntegral dlen))

foreign import ccall unsafe "hs_text_icu.h __hs_u_shapeArabic" u_shapeArabic
  :: Ptr UChar -> Int32
  -> Ptr UChar -> Int32
  -> Int32 -> Ptr UErrorCode -> IO Int32

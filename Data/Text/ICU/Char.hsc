{-# LANGUAGE DeriveDataTypeable, ForeignFunctionInterface #-}

-- |
-- Module      : Data.Text.ICU.Char
-- Copyright   : (c) 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Access to the Unicode Character Database, implemented as bindings
-- to the International Components for Unicode (ICU) libraries.
module Data.Text.ICU.Char
    (
      BlockCode(..)
    , Direction(..)
    -- * Functions
    , blockCode
    , charFullName
    , charName
    , combiningClass
    , digitToInt
    , direction
    , isMirrored
    , mirror
    ) where

#include <unicode/uchar.h>

import Control.Exception (throw)
import Data.Char (chr, ord)
import Data.Int (Int32)
import Data.Text.ICU.Internal (UBool, UChar32, asBool)
import Data.Text.ICU.Error (isFailure, u_BUFFER_OVERFLOW_ERROR)
import Data.Text.ICU.Error.Internal (UErrorCode, withError)
import Data.Typeable (Typeable)
import Data.Word (Word8)
import Foreign.C.String (CString, peekCStringLen)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr)
import System.IO.Unsafe (unsafePerformIO)

-- | The language directional property of a character set.
data Direction =
    LeftToRight
  | RightToLeft
  | EuropeanNumber
  | EuropeanNumberSeparator
  | EuropeanNumberTerminator
  | ArabicNumber
  | CommonNumberSeparator
  | BlockSeparator
  | SegmentSeparator
  | WhiteSpaceNeutral
  | OtherNeutral
  | LeftToRightEmbedding
  | LeftToRightOverride
  | RightToLeftArabic
  | RightToLeftEmbedding
  | RightToLeftOverride
  | PopDirectionalFormat
  | DirNonSpacingMark
  | BoundaryNeutral
  deriving (Eq, Enum, Bounded, Show, Typeable)

-- | Descriptions of Unicode blocks.
data BlockCode =
    NoBlock
  | BasicLatin
  | Latin1Supplement
  | LatinExtendedA
  | LatinExtendedB
  | IPAExtensions
  | SpacingModifierLetters
  | CombiningDiacriticalMarks
  | GreekAndCoptic
  | Cyrillic
  | Armenian
  | Hebrew
  | Arabic
  | Syriac
  | Thaana
  | Devanagari
  | Bengali
  | Gurmukhi
  | Gujarati
  | Oriya
  | Tamil
  | Telugu
  | Kannada
  | Malayalam
  | Sinhala
  | Thai
  | Lao
  | Tibetan
  | Myanmar
  | Georgian
  | HangulJamo
  | Ethiopic
  | Cherokee
  | UnifiedCanadianAboriginalSyllabics
  | Ogham
  | Runic
  | Khmer
  | Mongolian
  | LatinExtendedAdditional
  | GreekExtended
  | GeneralPunctuation
  | SuperscriptsAndSubscripts
  | CurrencySymbols
  | CombiningDiacriticalMarksForSymbols
  | LetterlikeSymbols
  | NumberForms
  | Arrows
  | MathematicalOperators
  | MiscellaneousTechnical
  | ControlPictures
  | OpticalCharacterRecognition
  | EnclosedAlphanumerics
  | BoxDrawing
  | BlockElements
  | GeometricShapes
  | MiscellaneousSymbols
  | Dingbats
  | BraillePatterns
  | CJKRadicalsSupplement
  | KangxiRadicals
  | IdeographicDescriptionCharacters
  | CJKSymbolsAndPunctuation
  | Hiragana
  | Katakana
  | Bopomofo
  | HangulCompatibilityJamo
  | Kanbun
  | BopomofoExtended
  | EnclosedCJKLettersAndMonths
  | CJKCompatibility
  | CJKUnifiedIdeographsExtensionA
  | CJKUnifiedIdeographs
  | YiSyllables
  | YiRadicals
  | HangulSyllables
  | HighSurrogates
  | HighPrivateUseSurrogates
  | LowSurrogates
  | PrivateUseArea
  | CJKCompatibilityIdeographs
  | AlphabeticPresentationForms
  | ArabicPresentationFormsA
  | CombiningHalfMarks
  | CJKCompatibilityForms
  | SmallFormVariants
  | ArabicPresentationFormsB
  | Specials
  | HalfwidthAndFullwidthForms
  | OldItalic
  | Gothic
  | Deseret
  | ByzantineMusicalSymbols
  | MusicalSymbols
  | MathematicalAlphanumericSymbols
  | CJKUnifiedIdeographsExtensionB 
  | CJKCompatibilityIdeographsSupplement
  | Tags
  | CyrillicSupplement
  | Tagalog
  | Hanunoo
  | Buhid
  | Tagbanwa
  | MiscellaneousMathematicalSymbolsA
  | SupplementalArrowsA
  | SupplementalArrowsB
  | MiscellaneousMathematicalSymbolsB
  | SupplementalMathematicalOperators
  | KatakanaPhoneticExtensions
  | VariationSelectors
  | SupplementaryPrivateUseAreaA
  | SupplementaryPrivateUseAreaB
  | Limbu
  | TaiLe
  | KhmerSymbols
  | PhoneticExtensions
  | MiscellaneousSymbolsAndArrows
  | YijingHexagramSymbols
  | LinearBSyllabary
  | LinearBIdeograms
  | AegeanNumbers
  | Ugaritic
  | Shavian
  | Osmanya
  | CypriotSyllabary
  | TaiXuanJingSymbols
  | VariationSelectorsSupplement
  | AncientGreekMusicalNotation
  | AncientGreekNumbers
  | ArabicSupplement
  | Buginese
  | CJKStrokes
  | CombiningDiacriticalMarksSupplement
  | Coptic
  | EthiopicExtended
  | EthiopicSupplement
  | GeorgianSupplement
  | Glagolitic
  | Kharoshthi
  | ModifierToneLetters
  | NewTaiLue
  | OldPersian
  | PhoneticExtensionsSupplement
  | SupplementalPunctuation
  | SylotiNagri
  | Tifinagh
  | VerticalForms
  | N'Ko
  | Balinese
  | LatinExtendedC
  | LatinExtendedD
  | PhagsPa
  | Phoenician
  | Cuneiform
  | CuneiformNumbersAndPunctuation
  | CountingRodNumerals
  | Sundanese
  | Lepcha
  | OlChiki
  | CyrillicExtendedA
  | Vai
  | CyrillicExtendedB
  | Saurashtra
  | KayahLi
  | Rejang
  | Cham
  | AncientSymbols
  | PhaistosDisc
  | Lycian
  | Carian
  | Lydian
  | MahjongTiles
  | DominoTiles
  deriving (Eq, Enum, Bounded, Show, Typeable)

-- | Return the Unicode allocation block that contains the given
-- character.
blockCode :: Char -> BlockCode
blockCode = toEnum . fromIntegral . ublock_getCode . fromIntegral . ord
{-# INLINE blockCode #-}

-- | Returns the bidirectional category value for the code point,
-- which is used in the Unicode bidirectional algorithm (UAX #9
-- <http://www.unicode.org/reports/tr9/>).
direction :: Char -> Direction
direction = toEnum . fromIntegral . u_charDirection . fromIntegral . ord
{-# INLINE direction #-}

-- | Determines whether the code point has the @Bidi_Mirrored@
-- property.  This property is set for characters that are commonly
-- used in Right-To-Left contexts and need to be displayed with a
-- "mirrored" glyph.
isMirrored :: Char -> Bool
isMirrored = asBool . u_isMirrored . fromIntegral . ord
{-# INLINE isMirrored #-}

-- Map the specified character to a "mirror-image" character.
--
-- For characters with the @Bidi_Mirrored@ property, implementations
-- sometimes need a "poor man's" mapping to another Unicode (code
-- point) such that the default glyph may serve as the mirror image of
-- the default glyph of the specified character. This is useful for
-- text conversion to and from codepages with visual order, and for
-- displays without glyph selection capabilities.
--
-- The return value is another Unicode code point that may serve as a
-- mirror-image substitute, or the original character itself if there
-- is no such mapping or the character lacks the @Bidi_Mirrored@
-- property.
mirror :: Char -> Char
mirror = chr . fromIntegral . u_charMirror . fromIntegral . ord
{-# INLINE mirror #-}

combiningClass :: Char -> Int
combiningClass = fromIntegral . u_getCombiningClass . fromIntegral . ord
{-# INLINE combiningClass #-}

-- | Return the decimal digit value of a decimal digit character.
-- Such characters have the general category @Nd@ (decimal digit
-- numbers) and a @Numeric_Type@ of @Decimal@.
--
-- No digit values are returned for any Han characters, because Han
-- number characters are often used with a special Chinese-style
-- number format (with characters for powers of 10 in between) instead
-- of in decimal-positional notation.  Unicode 4 explicitly assigns
-- Han number characters a @Numeric_Type@ of @Numeric@ instead of
-- @Decimal@.
digitToInt :: Char -> Maybe Int
digitToInt c
    | i == -1   = Nothing
    | otherwise = Just $! fromIntegral i
  where i = u_charDigitValue . fromIntegral . ord $ c

-- | Return the name of a Unicode character.
--
-- The names of all unassigned characters are empty.
--
-- The name contains only "invariant" characters like A-Z, 0-9, space,
-- and \'-\'.
charName :: Char -> String
charName = charName' (#const U_UNICODE_CHAR_NAME)

-- | Return the full name of a Unicode character.
--
-- Compared to 'charName', this function gives each Unicode code point
-- a unique name.
charFullName :: Char -> String
charFullName = charName' (#const U_EXTENDED_CHAR_NAME)

charName' :: UCharNameChoice -> Char -> String
charName' choice c = unsafePerformIO $ loop 128
 where
  loop n =
    allocaBytes n $ \ptr -> do
      (err,r) <- withError $ u_charName (fromIntegral (ord c)) choice ptr
                                        (fromIntegral n)
      case undefined of
       _| err == u_BUFFER_OVERFLOW_ERROR -> loop (fromIntegral r)
        | isFailure err                  -> throw err
        | otherwise                      -> peekCStringLen (ptr,fromIntegral r)

type UBlockCode = CInt
type UCharDirection = CInt
type UCharNameChoice = CInt

foreign import ccall unsafe "hs_text_icu.h __hs_ublock_getCode" ublock_getCode
    :: UChar32 -> UBlockCode

foreign import ccall unsafe "hs_text_icu.h __hs_u_charDirection" u_charDirection
    :: UChar32 -> UCharDirection

foreign import ccall unsafe "hs_text_icu.h __hs_u_isMirrored" u_isMirrored
    :: UChar32 -> UBool

foreign import ccall unsafe "hs_text_icu.h __hs_u_charMirror" u_charMirror
    :: UChar32 -> UChar32

foreign import ccall unsafe "hs_text_icu.h __hs_u_getCombiningClass" u_getCombiningClass
    :: UChar32 -> Word8

foreign import ccall unsafe "hs_text_icu.h __hs_u_charDigitValue" u_charDigitValue
    :: UChar32 -> Int32

foreign import ccall unsafe "hs_text_icu.h __hs_u_charName" u_charName
    :: UChar32 -> UCharNameChoice -> CString -> Int32 -> Ptr UErrorCode
    -> IO Int32

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
    , direction
    , isMirrored
    , mirror
    ) where

import Data.Char (chr, ord)
import Data.Text.ICU.Internal (UBool, UChar32, asBool)
import Data.Typeable (Typeable)
import Foreign.C.Types (CInt)

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

type UBlockCode = CInt
type UCharDirection = CInt

foreign import ccall unsafe "hs_text_icu.h __hs_ublock_getCode" ublock_getCode
    :: UChar32 -> UBlockCode

foreign import ccall unsafe "hs_text_icu.h __hs_u_charDirection" u_charDirection
    :: UChar32 -> UCharDirection

foreign import ccall unsafe "hs_text_icu.h __hs_u_isMirrored" u_isMirrored
    :: UChar32 -> UBool

foreign import ccall unsafe "hs_text_icu.h __hs_u_charMirror" u_charMirror
    :: UChar32 -> UChar32

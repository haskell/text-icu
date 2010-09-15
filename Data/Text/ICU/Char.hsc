{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, ForeignFunctionInterface,
    FunctionalDependencies, MultiParamTypeClasses #-}

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
    -- * Character property types
    -- ** Property identifier types
    , Property
    , BidiClass_(..)
    , Block_(..)
    , Bool_(..)
    , CanonicalCombiningClass_(..)
    , LeadCanonicalCombiningClass_(..)
    , TrailingCanonicalCombiningClass_(..)
    , EastAsianWidth_(..)
    , GeneralCategory_(..)
    , GraphemeClusterBreak_(..)
    , HangulSyllableType_(..)
    , JoiningGroup_(..)
    , JoiningType_(..)
    , LineBreak_(..)
    , NFCQuickCheck_(..)
    , NFDQuickCheck_(..)
    , NFKCQuickCheck_(..)
    , NFKDQuickCheck_(..)
    , NumericType_(..)
    -- ** Property value types
    , EastAsianWidth(..)
    , GeneralCategory(..)
    , GraphemeClusterBreak(..)
    , HangulSyllableType(..)
    , JoiningGroup(..)
    , JoiningType(..)
    , NumericType(..)
    -- * Functions
    , blockCode
    , charFullName
    , charName
    , combiningClass
    , digitToInt
    , direction
    , property
    , isoComment
    , isMirrored
    , mirror
    ) where

#include <unicode/uchar.h>

import Control.Exception (throw)
import Data.Char (chr, ord)
import Data.Int (Int32)
import Data.Text.ICU.Error (isFailure, u_BUFFER_OVERFLOW_ERROR)
import Data.Text.ICU.Error.Internal (UErrorCode, withError)
import Data.Text.ICU.Internal (UBool, UChar32, asBool)
import Data.Text.ICU.Normalize.Internal (toNCR)
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
  deriving (Eq, Enum, Show, Typeable)

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
  deriving (Eq, Enum, Show, Typeable)

data Bool_ =
    Alphabetic
  | ASCIIHexDigit
  -- ^ 0-9, A-F, a-f
  | BidiControl
  -- ^ Format controls which have specific functions in the Bidi Algorithm.
  | BidiMirrored
  -- ^ Characters that may change display in RTL text.
  | Dash
  -- ^ Variations of dashes.
  | DefaultIgnorable
  -- ^ Ignorable in most processing.
  | Deprecated
  -- ^ The usage of deprecated characters is strongly discouraged.
  | Diacritic
  -- ^ Characters that linguistically modify the meaning of another
  -- character to which they apply.
  | Extender
  -- ^ Extend the value or shape of a preceding alphabetic character,
  -- e.g. length and iteration marks.
  | FullCompositionExclusion
  | GraphemeBase
  -- ^ For programmatic determination of grapheme cluster boundaries.
  | GraphemeExtend
  -- ^ For programmatic determination of grapheme cluster boundaries.
  | GraphemeLink
  -- ^ For programmatic determination of grapheme cluster boundaries.
  | HexDigit
  -- ^ Characters commonly used for hexadecimal numbers.
  | Hyphen
  -- ^ Dashes used to mark connections between pieces of words, plus the
  -- Katakana middle dot.
  | IDContinue
  -- ^ Characters that can continue an identifier.
  | IDStart
  -- ^ Characters that can start an identifier.
  | Ideographic
  -- ^ CJKV ideographs.
  | IDSBinaryOperator
  -- ^ For programmatic determination of Ideographic Description Sequences.
  | IDSTrinaryOperator
  | JoinControl
  -- ^ Format controls for cursive joining and ligation.
  | LogicalOrderException
  -- ^ Characters that do not use logical order and require special handling
  -- in most processing.
  | Lowercase
  | Math
  | NonCharacter
  -- ^ Code points that are explicitly defined as illegal for the encoding
  -- of characters.
  | QuotationMark
  | Radical
  -- ^ For programmatic determination of Ideographic Description Sequences.
  | SoftDotted
  -- ^ Characters with a "soft dot", like i or j. An accent placed on these
  -- characters causes the dot to disappear.
  | TerminalPunctuation
  -- ^ Punctuation characters that generally mark the end of textual units.
  | UnifiedIdeograph
  -- ^ For programmatic determination of Ideographic Description Sequences.
  | Uppercase
  | WhiteSpace
  | XidContinue
  -- ^ 'IDContinue' modified to allow closure under normalization forms
  -- NFKC and NFKD.
  | XidStart
  -- ^ 'IDStart' modified to allow closure under normalization forms NFKC
  -- and NFKD.
  | CaseSensitive
  -- ^ Either the source of a case mapping or /in/ the target of a case
  -- mapping. Not the same as the general category @Cased_Letter@.
  | STerm
  -- ^ Sentence Terminal. Used in UAX #29: Text Boundaries
  -- <http://www.unicode.org/reports/tr29/>.
  | VariationSelector
  -- ^ Indicates all those characters that qualify as Variation
  -- Selectors. For details on the behavior of these characters, see
  -- <http://unicode.org/Public/UNIDATA/StandardizedVariants.html> and 15.6
  -- Variation Selectors.
  | NFDInert
  -- ^ ICU-specific property for characters that are inert under NFD, i.e.
  -- they do not interact with adjacent characters.  Used for example in
  -- normalizing transforms in incremental mode to find the boundary of
  -- safely normalizable text despite possible text additions.
  | NFKDInert
  -- ^ ICU-specific property for characters that are inert under NFKD, i.e.
  -- they do not interact with adjacent characters.
  | NFCInert
  -- ^ ICU-specific property for characters that are inert under NFC,
  -- i.e. they do not interact with adjacent characters.
  | NFKCInert
  -- ^ ICU-specific property for characters that are inert under NFKC,
  -- i.e. they do not interact with adjacent characters.
  | SegmentStarter
  -- ^ ICU-specific property for characters that are starters in terms of
  -- Unicode normalization and combining character sequences.
  | PatternSyntax
  -- ^ See UAX #31 Identifier and Pattern Syntax
  -- <http://www.unicode.org/reports/tr31/>.
  | PatternWhiteSpace
  -- ^ See UAX #31 Identifier and Pattern Syntax
  -- <http://www.unicode.org/reports/tr31/>.
  | POSIXAlNum
  -- ^ Alphanumeric character class.
  | POSIXBlank
  -- ^ Blank character class.
  | POSIXGraph
  -- ^ Graph character class.
  | POSIXPrint
  -- ^ Printable character class.
  | POSIXXDigit
  -- ^ Hex digit character class.
    deriving (Eq, Enum, Show, Typeable)

class Property p v | p -> v where
    fromNative :: p -> Int32 -> v
    toUProperty :: p -> UProperty

data BidiClass_ = BidiClass deriving (Show, Typeable)

instance Property BidiClass_ Direction where
    fromNative _  = toEnum . fromIntegral
    toUProperty _ = (#const UCHAR_BIDI_CLASS)

data Block_ = Block

instance Property Block_ BlockCode where
    fromNative _  = toEnum . fromIntegral
    toUProperty _ = (#const UCHAR_BLOCK)

data CanonicalCombiningClass_ = CanonicalCombiningClass deriving (Show,Typeable)

instance Property CanonicalCombiningClass_ Int where
    fromNative _  = fromIntegral
    toUProperty _ = (#const UCHAR_CANONICAL_COMBINING_CLASS)

data Decomposition_ = Decomposition deriving (Show, Typeable)

data Decomposition =
    Canonical
  | Compat
  | Circle
  | Final
  | Font
  | Fraction
  | Initial
  | Isolated
  | Medial
  | Narrow
  | Nobreak
  | Small
  | Square
  | Sub
  | Super
  | Vertical
  | Wide
  | Count
    deriving (Eq, Enum, Show, Typeable)

instance Property Decomposition_ (Maybe Decomposition) where
    fromNative _  = maybeEnum
    toUProperty _ = (#const UCHAR_DECOMPOSITION_TYPE)

data EastAsianWidth_ = EastAsianWidth deriving (Show, Typeable)

data EastAsianWidth = EANeutral
                    | EAAmbiguous
                    | EAHalf
                    | EAFull
                    | EANarrow
                    | EAWide
                    | EACount
                    deriving (Eq, Enum, Show, Typeable)

instance Property EastAsianWidth_ EastAsianWidth where
    fromNative _  = toEnum . fromIntegral
    toUProperty _ = (#const UCHAR_EAST_ASIAN_WIDTH)

instance Property Bool_ Bool where
    fromNative _ = (/=0)
    toUProperty  = fromIntegral . fromEnum

data GeneralCategory_ = GeneralCategory deriving (Show, Typeable)

data GeneralCategory =
    GeneralOtherType
  | UppercaseLetter
  | LowercaseLetter
  | TitlecaseLetter
  | ModifierLetter
  | OtherLetter
  | NonSpacingMark
  | EnclosingMark
  | CombiningSpacingMark
  | DecimalDigitNumber
  | LetterNumber
  | OtherNumber
  | SpaceSeparator
  | LineSeparator
  | ParagraphSeparator
  | ControlChar
  | FormatChar
  | PrivateUseChar
  | Surrogate
  | DashPunctuation
  | StartPunctuation
  | EndPunctuation
  | ConnectorPunctuation
  | OtherPunctuation
  | MathSymbol
  | CurrencySymbol
  | ModifierSymbol
  | OtherSymbol
  | InitialPunctuation
  | FinalPunctuation
    deriving (Eq, Enum, Show, Typeable)

instance Property GeneralCategory_ GeneralCategory where
    fromNative _  = toEnum . fromIntegral
    toUProperty _ = (#const UCHAR_GENERAL_CATEGORY)

data JoiningGroup_ = JoiningGroup deriving (Show, Typeable)

maybeEnum :: Enum a => Int32 -> Maybe a
maybeEnum 0 = Nothing
maybeEnum n = Just $! toEnum (fromIntegral n-1)

data JoiningGroup =
    Ain
  | Alaph
  | Alef
  | Beh
  | Beth
  | Dal
  | DalathRish
  | E
  | Feh
  | FinalSemkath
  | Gaf
  | Gamal
  | Hah
  | HamzaOnHehGoal
  | He
  | Heh
  | HehGoal
  | Heth
  | Kaf
  | Kaph
  | KnottedHeh
  | Lam
  | Lamadh
  | Meem
  | Mim
  | Noon
  | Nun
  | Pe
  | Qaf
  | Qaph
  | Reh
  | ReversedPe
  | Sad
  | Sadhe
  | Seen
  | Semkath
  | Shin
  | SwashKaf
  | SyriacWaw
  | Tah
  | Taw
  | TehMarbuta
  | Teth
  | Waw
  | Yeh
  | YehBarree
  | YehWithTail
  | Yudh
  | YudhHe
  | Zain
  | Fe
  | Khaph
  | Zhain
  | BurushaskiYehBarree
    deriving (Eq, Enum, Show, Typeable)

instance Property JoiningGroup_ (Maybe JoiningGroup) where
    fromNative _  = maybeEnum
    toUProperty _ = (#const UCHAR_JOINING_GROUP)

data JoiningType_ = JoiningType deriving (Show, Typeable)

data JoiningType =
    JoinCausing
  | DualJoining
  | LeftJoining
  | RightJoining
  | Transparent
    deriving (Eq, Enum, Show, Typeable)

instance Property JoiningType_ (Maybe JoiningType) where
    fromNative _  = maybeEnum
    toUProperty _ = (#const UCHAR_JOINING_TYPE)

data LineBreak_ = LineBreak deriving (Show, Typeable)

data LineBreak =
    Ambiguous
  | LBAlphabetic
  | BreakBoth
  | BreakAfter
  | BreakBefore
  | MandatoryBreak
  | ContingentBreak
  | ClosePunctuation
  | CombiningMark
  | CarriageReturn
  | Exclamation
  | Glue
  | LBHyphen
  | LBIdeographic
  | Inseparable
  | InfixNumeric
  | LineFeed
  | Nonstarter
  | Numeric
  | OpenPunctuation
  | PostfixNumeric
  | PrefixNumeric
  | Quotation
  | ComplexContext
  | LBSurrogate
  | Space
  | BreakSymbols
  | Zwspace
  | NextLine
  | WordJoiner
  | H2
  | H3
  | JL
  | JT
  | JV
    deriving (Eq, Enum, Show, Typeable)

instance Property LineBreak_ (Maybe LineBreak) where
    fromNative _  = maybeEnum
    toUProperty _ = (#const UCHAR_LINE_BREAK)

data NumericType_ = NumericType deriving (Show, Typeable)

data NumericType = NTDecimal | NTDigit | NTNumeric
                   deriving (Eq, Enum, Show, Typeable)

instance Property NumericType_ (Maybe NumericType) where
    fromNative _  = maybeEnum
    toUProperty _ = (#const UCHAR_NUMERIC_TYPE)

data HangulSyllableType_ = HangulSyllableType deriving (Show, Typeable)

data HangulSyllableType =
    LeadingJamo
  | VowelJamo
  | TrailingJamo
  | LVSyllable
  | LVTSyllable
    deriving (Eq, Enum, Show, Typeable)

instance Property HangulSyllableType_ (Maybe HangulSyllableType) where
    fromNative _  = maybeEnum
    toUProperty _ = (#const UCHAR_HANGUL_SYLLABLE_TYPE)

data NFCQuickCheck_ = NFCQuickCheck deriving (Show, Typeable)
data NFDQuickCheck_ = NFDQuickCheck deriving (Show, Typeable)
data NFKCQuickCheck_ = NFKCQuickCheck deriving (Show, Typeable)
data NFKDQuickCheck_ = NFKDQuickCheck deriving (Show, Typeable)

instance Property NFCQuickCheck_ (Maybe Bool) where
    fromNative  _ = toNCR . fromIntegral
    toUProperty _ = (#const UCHAR_NFC_QUICK_CHECK)

instance Property NFDQuickCheck_ (Maybe Bool) where
    fromNative  _ = toNCR . fromIntegral
    toUProperty _ = (#const UCHAR_NFD_QUICK_CHECK)

instance Property NFKCQuickCheck_ (Maybe Bool) where
    fromNative  _ = toNCR . fromIntegral
    toUProperty _ = (#const UCHAR_NFKC_QUICK_CHECK)

instance Property NFKDQuickCheck_ (Maybe Bool) where
    fromNative  _ = toNCR . fromIntegral
    toUProperty _ = (#const UCHAR_NFKD_QUICK_CHECK)

data LeadCanonicalCombiningClass_ = LeadCanonicalCombiningClass
                                    deriving (Show, Typeable)

instance Property LeadCanonicalCombiningClass_ Int where
    fromNative  _ = fromIntegral
    toUProperty _ = (#const UCHAR_LEAD_CANONICAL_COMBINING_CLASS)

data TrailingCanonicalCombiningClass_ = TrailingCanonicalCombiningClass
                                   deriving (Show, Typeable)

instance Property TrailingCanonicalCombiningClass_ Int where
    fromNative  _ = fromIntegral
    toUProperty _ = (#const UCHAR_TRAIL_CANONICAL_COMBINING_CLASS)

data GraphemeClusterBreak_ = GraphemeClusterBreak deriving (Show, Typeable)

data GraphemeClusterBreak =
    Control
  | CR
  | Extend
  | L
  | LF
  | LV
  | LVT
  | T
  | V
  | SpacingMark
  | Prepend
    deriving (Eq, Enum, Show, Typeable)

instance Property GraphemeClusterBreak_ (Maybe GraphemeClusterBreak) where
    fromNative  _ = maybeEnum
    toUProperty _ = (#const UCHAR_GRAPHEME_CLUSTER_BREAK)

property :: Property p v => p -> Char -> v
property p c = fromNative p . u_getIntPropertyValue (fromIntegral (ord c)) .
               toUProperty $ p
{-# INLINE property #-}

-- | Return the Unicode allocation block that contains the given
-- character.
blockCode :: Char -> BlockCode
blockCode = toEnum . fromIntegral . ublock_getCode . fromIntegral . ord
{-# INLINE blockCode #-}

-- | Return the bidirectional category value for the code point,
-- which is used in the Unicode bidirectional algorithm (UAX #9
-- <http://www.unicode.org/reports/tr9/>).
direction :: Char -> Direction
direction = toEnum . fromIntegral . u_charDirection . fromIntegral . ord
{-# INLINE direction #-}

-- | Determine whether the code point has the @Bidi_Mirrored@
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

-- | Return the ISO 10646 comment for a character.
--
-- If a character does not have an associated comment, the empty
-- string is returned.
--
-- The ISO 10646 comment is an informative field in the Unicode
-- Character Database (@UnicodeData.txt@ field 11) and is from the ISO
-- 10646 names list.
isoComment :: Char -> String
isoComment c = fillString $ u_getISOComment (fromIntegral (ord c))

charName' :: UCharNameChoice -> Char -> String
charName' choice c = fillString $ u_charName (fromIntegral (ord c)) choice

fillString :: (CString -> Int32 -> Ptr UErrorCode -> IO Int32) -> String
fillString act = unsafePerformIO $ loop 128
 where
  loop n =
    allocaBytes n $ \ptr -> do
      (err,r) <- withError $ act ptr (fromIntegral n)
      case undefined of
       _| err == u_BUFFER_OVERFLOW_ERROR -> loop (fromIntegral r)
        | isFailure err                  -> throw err
        | otherwise                      -> peekCStringLen (ptr,fromIntegral r)

type UBlockCode = CInt
type UCharDirection = CInt
type UCharNameChoice = CInt
type UProperty = CInt

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

foreign import ccall unsafe "hs_text_icu.h __hs_u_getISOComment" u_getISOComment
    :: UChar32 -> CString -> Int32 -> Ptr UErrorCode -> IO Int32

foreign import ccall unsafe "hs_text_icu.h __hs_u_getIntPropertyValue" u_getIntPropertyValue
    :: UChar32 -> UProperty -> Int32

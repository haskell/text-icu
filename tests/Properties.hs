-- Tester beware!
--
-- Many of the tests below are "weak", i.e. they ensure that functions
-- return results, without checking whether the results are correct.
-- Weak tests are described as such.

{-# LANGUAGE OverloadedStrings, LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Properties (propertyTests, testCases) where

import Control.DeepSeq (NFData(..))
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.ICU (LocaleName(..))
import QuickCheckUtils (NonEmptyText(..), LatinSpoofableText(..),
                        NonSpoofableText(..), Utf8Text(..))
import Data.Text.ICU.Normalize2 (NormalizationMode(..))
import qualified Data.Text.ICU.Normalize2 as I
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit ((~?=), (@?=))
import qualified Test.HUnit (Test(..))
import Test.QuickCheck.Monadic (monadicIO, run, assert)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.ICU as I
import qualified Data.Text.ICU.BiDi as BiDi
import qualified Data.Text.ICU.Calendar as Cal
import qualified Data.Text.ICU.Convert as I
import qualified Data.Text.ICU.Char as I
import qualified Data.Text.ICU.CharsetDetection as CD
import qualified Data.Text.ICU.Number as N
import qualified Data.Text.ICU.Shape as S
import System.IO.Unsafe (unsafePerformIO)

{-# ANN module ("HLint: use camelCase"::String) #-}

t_rnf :: (NFData b) => (a -> b) -> a -> Bool
t_rnf f t = rnf (f t) == ()

t_nonEmpty :: (Text -> Text) -> Text -> Bool
t_nonEmpty f t
    | T.null t  = T.null ft
    | otherwise = T.length ft > 0
  where ft = f t

-- Case mapping

-- These tests are all fairly weak.

t_toCaseFold bool = t_nonEmpty $ I.toCaseFold bool
t_toLower locale = t_nonEmpty $ I.toLower locale
t_toUpper locale = t_nonEmpty $ I.toUpper locale

-- Iteration

t_charIterator_String a b = (compare `on` I.fromString) a b == compare a b
t_charIterator_Text a b = (compare `on` I.fromText) a b == compare a b
t_charIterator_Utf8 a b = (compare `on` I.fromUtf8) ba bb == compare ba bb
  where ba = T.encodeUtf8 a; bb = T.encodeUtf8 b

-- Normalization

t_normalize mode = t_nonEmpty $ I.normalize mode

t_quickCheck_isNormalized mode normMode txt
  | mode `elem` [NFD, NFKD] =          quickCheck == Just isNormalized
  | otherwise = fromMaybe isNormalized quickCheck ==      isNormalized
  where quickCheck   = I.quickCheck mode normTxt
        isNormalized = I.isNormalized mode normTxt
        normTxt      = I.normalize normMode txt

-- Collation

t_collate a b = c a b == flipOrdering (c b a)
    where c = I.collate I.uca

flipOrdering :: Ordering -> Ordering
flipOrdering = \ case
    GT -> LT
    LT -> GT
    EQ -> EQ

-- Convert

converter e = unsafePerformIO $ I.open e Nothing

t_convert a = I.toUnicode c (I.fromUnicode c a) == a
    where c = converter "UTF-32"


-- Unicode character database

-- These tests are weak.

t_blockCode = t_rnf I.blockCode
t_charFullName c = I.charFromFullName (I.charFullName c) == Just c
t_charName c = maybe True (==c) $ I.charFromName (I.charName c)
t_combiningClass = t_rnf I.combiningClass
t_direction = t_rnf I.direction
-- t_property p = t_rnf $ I.property p
t_isMirrored = t_rnf $ I.isMirrored
t_mirror = t_rnf $ I.mirror
t_digitToInt = t_rnf $ I.digitToInt
t_numericValue = t_rnf $ I.numericValue

-- Spoofing

t_nonspoofable (NonSpoofableText t) = I.spoofCheck I.spoof t == I.CheckOK
t_spoofable (LatinSpoofableText t) = I.spoofCheck I.spoof t ==
                                     I.CheckFailed [I.RestrictionLevel]
t_confusable (NonEmptyText t) = I.areConfusable I.spoof t t `elem`
  [I.CheckFailed [I.MixedScriptConfusable]
  ,I.CheckFailed [I.SingleScriptConfusable]]

-- Encoding Guessing

t_Utf8IsUtf8 a = monadicIO $ do
    val <- run $ CD.detect (utf8Text a) >>= CD.getName
    assert $ T.isPrefixOf "UTF-8" val

propertyTests :: Test
propertyTests =
  testGroup "Properties" [
    testProperty "t_toCaseFold" t_toCaseFold
  , testProperty "t_toLower" t_toLower
  , testProperty "t_toUpper" t_toUpper
  , testProperty "t_charIterator_String" t_charIterator_String
  , testProperty "t_charIterator_Text" t_charIterator_Text
  , testProperty "t_charIterator_Utf8" t_charIterator_Utf8
  , testProperty "t_normalize" t_normalize
  , testProperty "t_quickCheck_isNormalized" t_quickCheck_isNormalized
  , testProperty "t_collate" t_collate
  , testProperty "t_convert" t_convert
  , testProperty "t_blockCode" t_blockCode
  , testProperty "t_charFullName" t_charFullName
  , testProperty "t_charName" t_charName
  , testProperty "t_combiningClass" t_combiningClass
  , testProperty "t_direction" $ t_direction
--, testProperty "t_property" t_property
  , testProperty "t_isMirrored" t_isMirrored
  , testProperty "t_mirror" t_mirror
  , testProperty "t_digitToInt" t_digitToInt
  , testProperty "t_numericValue" t_numericValue
  , testProperty "t_spoofable" t_spoofable
  , testProperty "t_nonspoofable" t_nonspoofable
  , testProperty "t_confusable" t_confusable
  , testProperty "t_Utf8IsUtf8" t_Utf8IsUtf8
  ]

testCases :: Test
testCases =
  testGroup "Test cases" $ hUnitTestToTests $ Test.HUnit.TestList $
  [I.normalize NFC "Ame\x0301lie" ~?= "Amélie"
  ,I.normalize NFC "(⊃｡•́︵•̀｡)⊃" ~?= "(⊃｡•́︵•̀｡)⊃"
  ,map I.brkBreak (I.breaks (I.breakWord (Locale "en_US")) "Hi, Amélie!")
     ~?= ["Hi",","," ","Amélie","!"]
  ,map I.brkBreak (I.breaksRight (I.breakLine (Locale "ru")) "Привет, мир!")
     ~?= ["мир!","Привет, "]
  ,(I.unfold I.group <$> I.findAll "[abc]+" "xx b yy ac") ~?= [["b"],["ac"]]
  ,I.toUpper (Locale "de-DE") "ß" ~?= "SS"
  ,I.toCaseFold False "ﬂag" ~?= "flag"
  ,I.blockCode '\x1FA50' ~?= I.ChessSymbols
  ,I.direction '\x2068' ~?= I.FirstStrongIsolate
  ,I.getSkeleton I.spoof Nothing "\1089\1072t" ~?= "cat"
  ,S.shapeArabic [S.LettersShape] (nosp "ا ب ت ث") ~?= (nosp "ﺍ ﺑ ﺘ ﺚ")
  ,BiDi.reorderParagraphs [] (nosp "abc ا ب ت ث def\n123")
     ~?= ["abc" <> T.reverse (nosp "ا ب ت ث") <> "def\n", "123"]
  ,N.formatNumber (N.numberFormatter N.NUM_CURRENCY_PLURAL "en_US")
     (12.5 :: Double) ~?= "12.50 US dollars"

  ,do
     dfDe <- I.standardDateFormatter I.LongFormatStyle I.LongFormatStyle
       (Locale "de_DE") ""
     c <- cal "CET" 2000 00 01 02 03 00
     return $ I.formatCalendar dfDe (Cal.add c [(Cal.Hour, 25), (Cal.Second, 65)])
   `ioEq`
    "2. Januar 2000 um 03:04:05 GMT+1"

  ,do
     dfAt <- I.standardDateFormatter I.LongFormatStyle I.LongFormatStyle
       (Locale "de_AT") "CET"
     return $ I.dateSymbols dfAt I.Months
   `ioEq`
   ["Jänner","Februar","März","April","Mai","Juni"
   ,"Juli","August","September","Oktober","November","Dezember"]

  ,do
     dfP <- I.patternDateFormatter
       "MMMM dd, yyyy GGGG, hh 'o''clock' a, VVVV" (Locale "en_US") ""
     c <- cal "America/Los_Angeles" 2000 00 02 03 04 05
     return $ I.formatCalendar dfP c
   `ioEq`
    "January 02, 2000 Anno Domini, 03 o'clock AM, Los Angeles Time"

  ,(flip Cal.getField Cal.Year =<< cal "UTC" 1999 01 02 03 04 05) `ioEq` 1999

  ,(elem "en_US" <$> I.availableLocales) `ioEq` True

  ,(flip I.formatIntegral (12345 :: Int)
    <$> I.numberFormatter "precision-integer" (Locale "fr"))
   `ioEq` "12\8239\&345"

  ,(flip I.formatDouble 12345.6789
    <$> I.numberFormatter "precision-currency-cash currency/EUR" (Locale "it"))
   `ioEq` "12.345,68\160€"

  ]
  <>
  concat
  [conv "ISO-2022-CN" "程序設計"  "\ESC$)A\SO3LPr\ESC$)G]CSS\SI"
  ,conv "cp1251" "Привет, мир!"  "\207\240\232\226\229\242, \236\232\240!"
  ]
  where conv n f t = [I.fromUnicode c f ~?= t, I.toUnicode c t ~?= f]
            where c = converter n
        nosp = T.filter (/= ' ')
        cal tz y m d h mn s = do
            c <- Cal.calendar tz (Locale "en_US") Cal.TraditionalCalendarType
            Cal.setDateTime c y m d h mn s
            return c
        ioEq io a = Test.HUnit.TestCase $ do
            x <- io
            x @?= a

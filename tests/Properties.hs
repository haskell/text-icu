-- Tester beware!
--
-- Many of the tests below are "weak", i.e. they ensure that functions
-- return results, without checking whether the results are correct.
-- Weak tests are described as such.

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Properties (tests) where

import Control.DeepSeq (NFData(..))
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.ICU (NormalizationMode(..))
import QuickCheckUtils ()
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.ICU as I

t_rnf :: (NFData a) => (Text -> a) -> Text -> Bool
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
  | mode `elem` [NFD, NFKD, FCD]
              =                        quickCheck == Just isNormalized
  | otherwise = fromMaybe isNormalized quickCheck ==      isNormalized
  where quickCheck   = I.quickCheck mode normTxt
        isNormalized = I.isNormalized mode normTxt
        normTxt      = I.normalize normMode txt

-- Collation

-- This test is weak.

t_collate_root txt = t_rnf $ I.collate I.uca txt


tests :: Test
tests =
  testGroup "Properties" [
    testProperty "t_toCaseFold" t_toCaseFold
  , testProperty "t_toLower" t_toLower
  , testProperty "t_toUpper" t_toUpper
  , testProperty "t_charIterator_String" t_charIterator_String
  , testProperty "t_charIterator_Text" t_charIterator_Text
  , testProperty "t_charIterator_Utf8" t_charIterator_Utf8
  , testProperty "t_normalize" t_normalize
  , testProperty "t_quickCheck_isNormalized" t_quickCheck_isNormalized
  , testProperty "t_collate_root" t_collate_root
  ]

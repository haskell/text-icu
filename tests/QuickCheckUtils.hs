{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module QuickCheckUtils (NonEmptyText(..), LatinSpoofableText(..),
                        NonSpoofableText(..), Utf8Text(..)) where

import Data.Text.ICU (Collator, LocaleName(..), NormalizationMode(..))
import Data.Text.ICU.Break (available)
import Test.QuickCheck (Arbitrary(..), Gen, elements, listOf1, suchThat, vectorOf)
import Data.Word (Word8)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.ICU as I

instance Arbitrary T.Text where
    arbitrary = T.pack `fmap` arbitrary
    shrink = map T.pack . shrink . T.unpack

instance Arbitrary BS.ByteString where
    arbitrary = BS.pack <$> arbitrary
    shrink xs = BS.pack <$> shrink (BS.unpack xs)

instance Arbitrary LocaleName where
    arbitrary = elements (Root:available)

instance Arbitrary NormalizationMode where
    arbitrary = elements [None ..FCD]

instance Arbitrary Collator where
    arbitrary = I.collator <$> arbitrary

newtype NonEmptyText = NonEmptyText { nonEmptyText :: T.Text } deriving Show

instance Arbitrary NonEmptyText where
  arbitrary = NonEmptyText <$> T.pack <$> listOf1 arbitrary

newtype LatinSpoofableText = LatinSpoofableText { latinSpoofableText :: T.Text }
                           deriving Show
instance Arbitrary LatinSpoofableText where
    arbitrary = LatinSpoofableText <$> T.pack . (<>) "latin" <$>
                listOf1 genCyrillicLatinSpoofableChar

genCyrillicLatinSpoofableChar :: Gen Char
genCyrillicLatinSpoofableChar = elements (
  "\x043A\x043E\x0433\x0435\x043A\x043C" ++
  ['\x043E'..'\x0443'] ++
  ['\x0445'..'\x0446'] ++
  "\x044A" ++
  ['\x0454'..'\x0456'] ++
  "\x0458\x045B\x048D\x0491\x0493\x049B\x049F\x04AB\x04AD\x04AF\x04B1\x04BB\
  \\x04BD\x04BF" ++
  ['\x04CE'..'\x04CF'] ++
  "\x04D5\x04D9\x04E9\x0501\x0511\x051B\x051D")

newtype NonSpoofableText = NonSpoofableText { nonSpoofableText :: T.Text }
                         deriving Show

instance Arbitrary NonSpoofableText where
    arbitrary = NonSpoofableText <$> T.pack <$> listOf1 genNonSpoofableChar

genNonSpoofableChar :: Gen Char
genNonSpoofableChar = elements "QDFRz"

newtype Utf8Text = Utf8Text { utf8Text :: BS.ByteString }
                 deriving Show

instance Arbitrary Utf8Text where
    arbitrary = Utf8Text . BS.pack <$> vectorOf 300
        (suchThat
            (arbitrary :: Gen Word8)
            (`elem` ([0x41..0x5A] ++ [0x61..0x7A]))
        )

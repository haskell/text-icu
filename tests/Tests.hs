module Main (main) where

import Test.Framework (defaultMain)

import qualified Properties

main :: IO ()
main = defaultMain [Properties.tests]

module Data.BTree.Array.Util.Tests
    ( tests
    ) where

import Data.List (nub, sort)

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified Data.BTree.Array as A
import qualified Data.BTree.Array.Util as A

tests :: Test
tests = testGroup "Data.BTree.Array.Util.Tests"
    [ testProperty "binarySearch" binarySearch
    ]

binarySearch :: [Int] -> Int -> Bool
binarySearch list x = case A.binarySearch length' x array of
    Nothing -> x `notElem` sorted
    Just i  -> sorted !! i == x
  where
    sorted  = sort $ nub list
    length' = length sorted
    array   = A.fromList sorted

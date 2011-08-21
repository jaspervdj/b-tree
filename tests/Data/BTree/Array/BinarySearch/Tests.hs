module Data.BTree.Array.BinarySearch.Tests
    ( tests
    ) where

import Data.List (nub, sort)

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.BTree.Array.BinarySearch
import qualified Data.BTree.Array as A

tests :: Test
tests = testGroup "Data.BTree.Array.BinarySearch.Tests"
    [ testProperty "binarySearch" t_binarySearch
    ]

t_binarySearch :: [Int] -> Int -> Bool
t_binarySearch list x = case binarySearch length' x array of
    Nothing -> x `notElem` sorted
    Just i  -> sorted !! i == x
  where
    sorted  = sort $ nub list
    length' = length sorted
    array   = A.fromList sorted

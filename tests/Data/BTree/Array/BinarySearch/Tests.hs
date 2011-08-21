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
    [ testProperty "binarySearch"     t_binarySearch
    , testProperty "binarySearchWith" t_binarySearchWith
    ]

t_binarySearch :: [Int] -> Int -> Bool
t_binarySearch list x = case binarySearch length' x array of
    Nothing -> x `notElem` sorted
    Just i  -> sorted !! i == x
  where
    sorted  = sort $ nub list
    length' = length sorted
    array   = A.fromList sorted

t_binarySearchWith :: [Int] -> Int -> Bool
t_binarySearchWith list x = case binarySearchWith Left Right length' x array of
    Left i  -> sorted !! i == x
    Right i -> x `notElem` sorted &&
        bound i (sorted !! i > x) &&
        bound (i - 1) (sorted !! (i - 1) < x)
  where
    sorted  = sort $ nub list
    length' = length sorted
    array   = A.fromList sorted

    bound i e = if i >= 0 && i < length' then e else True

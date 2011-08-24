module Data.BTree.NodeSearch.Tests
    ( tests
    ) where

import Data.List (nub, sort)

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.BTree.NodeSearch
import qualified Data.BTree.Array as A

tests :: Test
tests = testGroup "Data.BTree.NodeSearch.Tests"
    [ testProperty "search"     t_search
    , testProperty "searchWith" t_searchWith
    ]

t_search :: [Int] -> Int -> Bool
t_search list x = case search length' x array of
    Nothing -> x `notElem` sorted
    Just i  -> sorted !! i == x
  where
    sorted  = sort $ nub list
    length' = length sorted
    array   = A.fromList sorted

t_searchWith :: [Int] -> Int -> Bool
t_searchWith list x = case searchWith Left Right length' x array of
    Left i  -> sorted !! i == x
    Right i -> x `notElem` sorted &&
        bound i (sorted !! i > x) &&
        bound (i - 1) (sorted !! (i - 1) < x)
  where
    sorted  = sort $ nub list
    length' = length sorted
    array   = A.fromList sorted

    bound i e = if i >= 0 && i < length' then e else True

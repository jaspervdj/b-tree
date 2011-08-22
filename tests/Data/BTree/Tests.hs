module Data.BTree.Tests
    ( tests
    ) where

import qualified Data.Map as M

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified Data.BTree as B
import Data.BTree.Invariants
import Data.BTree.QuickCheck

tests :: Test
tests = testGroup "Data.BTree.Tests"
    [ testProperty "fromList" t_fromList
    ]

t_fromList :: [(Int, Int)] -> Bool
t_fromList assoc = btree `compareMap` M.fromList assoc && invariants btree
  where
    btree = B.fromList assoc

-- | QuickCheck utility functions for testing
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.BTree.QuickCheck
    ( compareMap
    ) where

import Data.Map (Map)
import qualified Data.Map as M

import Test.QuickCheck (Arbitrary (..))

import Data.BTree (BTree)
import qualified Data.BTree as B

instance (Arbitrary k, Arbitrary v, Ord k) => Arbitrary (BTree k v) where
    arbitrary = fmap B.fromList arbitrary

-- | Compare a btree to a map.
compareMap :: (Ord k, Eq v) => BTree k v -> Map k v -> Bool
compareMap btree = all check . M.toList
  where
    check (k, v) = B.lookup k btree == Just v

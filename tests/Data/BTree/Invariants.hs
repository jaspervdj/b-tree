-- | A number of invariant checks on the B-tree type
module Data.BTree.Invariants
    ( invariants
    , nodeSizeInvariant
    , balancingInvariant
    ) where

import Data.BTree.Internal
import qualified Data.BTree.Array as A

-- | Check all invariants
invariants :: BTree k v -> Bool
invariants btree = all ($ btree) [nodeSizeInvariant, balancingInvariant]

-- | Get the children of the root node as a list
rootChildren :: BTree k v -> [BTree k v]
rootChildren (Node _ _ _ N)     = []
rootChildren (Node s _ _ (J c)) = A.toList (s + 1) c

-- | Check if a tree is a leaf
isLeaf :: BTree k v -> Bool
isLeaf (Node _ _ _ N) = True
isLeaf _              = False

-- | Check that each node contains enough keys
nodeSizeInvariant :: BTree k v -> Bool
nodeSizeInvariant = all nodeSizeInvariant' . rootChildren
  where
    nodeSizeInvariant' btree =
        invariant (nodeSize btree) &&
        all nodeSizeInvariant' (rootChildren btree)

    invariant s
        | s >= maxNodeSize `div` 2 && s <= maxNodeSize = True
        | otherwise = False

-- | Check for perfect balancing
balancingInvariant :: BTree k v -> Bool
balancingInvariant = equal . depths
  where
    -- Check if all elements in a list are equal
    equal (x : y : t) = x == y && equal (y : t)
    equal _           = True
    
    -- Depths of all leaves
    depths tree
        | isLeaf tree = [0 :: Int]
        | otherwise   = rootChildren tree >>= map (+ 1) . depths

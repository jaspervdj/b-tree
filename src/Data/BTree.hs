{-# LANGUAGE BangPatterns #-}
module Data.BTree
    ( -- * Types
      BTree

      -- * Creation
    , empty
    , singleton

      -- * Searching
    , lookup

      -- * Insertion
    , insert

      -- * Debugging
    , showBTree
    ) where

import Prelude hiding (lookup)

import Control.Applicative ((<$>), (<*>))
import Control.Monad.ST (ST, runST)
import Control.Monad (liftM3, mapM, sequence)
import Data.List (intercalate)
import qualified Prelude as P
import qualified Data.Vector.Mutable as V

import Data.BTree.Array.BinarySearch
import qualified Data.BTree.Array as A
import qualified Data.BTree.Array.Util as A

maxNodeSize :: Int
maxNodeSize = 8
{-# INLINE maxNodeSize #-}

data BTree k v
    = Node
        { nodeSize      :: !Int
        , nodeTotalSize :: !Int
        , nodeKeys      :: !(A.Array k)
        , nodeChildren  :: !(A.Array (BTree k v))
        }
    | Leaf
        { nodeSize      :: !Int
        , nodeKeys      :: !(A.Array k)
        , nodeValues    :: !(A.Array v)
        }

-- | Create an empty 'BTree'
empty :: Ord k => BTree k v
empty = Leaf 0 A.empty A.empty
{-# INLINE empty #-}

-- | Create a 'BTree' holding a single element
singleton :: Ord k => k -> v -> BTree k v
singleton k v = Leaf 1 (A.singleton k) (A.singleton v)
{-# INLINE singleton #-}

-- | Find an element in the 'BTree'
lookup :: Ord k => k -> BTree k v -> Maybe v
lookup k = lookup'
  where
    lookup' (Leaf s ks vs) = fmap (A.unsafeIndex vs) (binarySearch s k ks)
    lookup' (Node s _ ks cs) = binarySearchWith found notFound s k ks
      where
        found i = lookup' (A.unsafeIndex cs (i + 1))
        {-# INLINE found #-}
        notFound i = lookup' (A.unsafeIndex cs i)
        {-# INLINE notFound #-}
{-# INLINE lookup #-}

-- | Insert an element into the 'BTree'
insert :: Ord k => k -> v -> BTree k v -> BTree k v
insert k v (Leaf s ks vs) = binarySearchWith found notFound s k ks
  where
    -- Overwrite the value
    found i  = Leaf s ks (A.unsafePut s i v vs)
    -- Insert the value
    -- TODO: check if size is big enough
    notFound i
        -- We have enough place, so just insert it
        | s + 1 <= maxNodeSize = Leaf
            (s + 1) (A.unsafeInsert s i k ks) (A.unsafeInsert s i v vs)
        -- We need to split this leaf and insert left
        | i < s' =
            let lks = A.unsafeInsertIn 0 s' i k ks
                lvs = A.unsafeInsertIn 0 s' i v vs
                rks = A.unsafeCopyRange s' rs ks
                rvs = A.unsafeCopyRange s' rs vs
                l = Leaf (s' + 1) lks lvs
                r = Leaf rs rks rvs
                ks = A.singleton (A.unsafeIndex rks 0)
                cs = A.pair l r
            in Node 1 (s + 1) ks cs
        -- We need to split this leaf and insert right
        | otherwise =
            let lks = A.unsafeCopyRange 0 s' ks
                lvs = A.unsafeCopyRange 0 s' vs
                rks = A.unsafeInsertIn s' rs (i - s') k ks
                rvs = A.unsafeInsertIn s' rs (i - s') v vs
                l = Leaf s' lks lvs
                r = Leaf (rs + 1) rks rvs
                ks = A.singleton (A.unsafeIndex rks 0)
                cs = A.pair l r
            in Node 1 (s + 1) ks cs
      where
        s' = (s `div` 2) + 1
        rs = s - s'
insert k v (Node s ts ks cs) = binarySearchWith found notFound s k ks
  where
    -- Found: right child
    -- TODO: update total size
    -- TODO: optimization: size does not change
    found i = let !c' = insert k v (A.unsafeIndex cs (i + 1))
              in Node s ts ks (A.unsafePut (s + 1) (i + 1) c' cs)

    -- Not found: left child
    notFound i = let !c' = insert k v (A.unsafeIndex cs i)
                 in Node s ts ks (A.unsafePut (s + 1) i c' cs)
{-# INLINE insert #-}

-- | Show the internal structure of a 'BTree', useful for debugging
showBTree :: (Show k, Show v) => BTree k v -> String
showBTree btree = unlines $ showBTree' btree
  where
    showBTree' b = case b of
        Node s _ _ _ -> concatMap (showElement b) [0 .. s]
        Leaf s _ _   -> map (showTuple b) [0 .. s - 1]

    showElement b i
        | i == nodeSize b = showChild b i
        | otherwise       = showChild b i ++ [showKey b i]

    showChild b i = map (++ "    ") $ showBTree' $
        A.unsafeIndex (nodeChildren b) i

    showKey b i = show $ A.unsafeIndex (nodeKeys b) i

    showTuple b i = show
        (A.unsafeIndex (nodeKeys b) i, A.unsafeIndex (nodeValues b) i)

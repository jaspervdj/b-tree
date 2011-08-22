{-# LANGUAGE BangPatterns, PatternGuards #-}
module Data.BTree
    ( -- * Types
      BTree

      -- * Creation
    , empty
    , singleton
    , fromList

      -- * Queries
    , size
    , lookup

      -- * Insertion
    , insert

      -- * Debugging
    , showBTree
    ) where

import Prelude hiding (lookup)

-- import qualified Prelude as P

import Data.List (foldl')

import Data.BTree.Array.BinarySearch
import qualified Data.BTree.Array as A
import qualified Data.BTree.Array.Util as A

maxNodeSize :: Int
maxNodeSize = 4
{-# INLINE maxNodeSize #-}

data BTree k v
    = Node
        { nodeSize        :: !Int
        , nodeTotalValues :: !Int
        , nodeKeys        :: !(A.Array k)
        , nodeChildren    :: !(A.Array (BTree k v))
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

-- | Create a 'BTree' from an associative list
fromList :: Ord k => [(k, v)] -> BTree k v
fromList = foldl' insert' empty
  where
    insert' t (k, v) = insert k v t

-- | Find the number of values in the 'BTree'
size :: BTree k v -> Int
size (Leaf s _ _)     = s
size (Node _ tv _ _ ) = tv

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

-- | Signals for insertion
data Insert k v = Ok !(BTree k v)
                | Split !(BTree k v) !(BTree k v)

-- | Insert an element into the 'BTree'
insert :: Ord k => k -> v -> BTree k v -> BTree k v
insert k v btree =
    -- Insertion in the root
    case insert' btree of
        Ok btree' -> btree'
        -- Split the root
        Split l r ->
            let !s = 1
                !tv = size l + size r
                !ks = A.singleton (A.unsafeIndex (nodeKeys r) 0)
                !cs = A.pair l r
            in Node s tv ks cs
  where
    -- Insertion in a leaf node
    insert' (Leaf s ks vs) = binarySearchWith found notFound s k ks
      where
        -- Overwrite the value
        found i = Ok $ Leaf s ks (A.unsafePut s i v vs)

        -- Insert the value
        notFound i
            -- We have enough place, so just insert it
            | s + 1 <= maxNodeSize = Ok $ Leaf
                (s + 1) (A.unsafeInsert s i k ks) (A.unsafeInsert s i v vs)
            -- We need to split this leaf and insert left
            | i < s' =
                let lks = A.unsafeInsertIn 0 s' i k ks
                    lvs = A.unsafeInsertIn 0 s' i v vs
                    rks = A.unsafeCopyRange s' rs ks
                    rvs = A.unsafeCopyRange s' rs vs
                    l = Leaf (s' + 1) lks lvs
                    r = Leaf rs rks rvs
                in Split l r
            -- We need to split this leaf and insert right
            | otherwise =
                let lks = A.unsafeCopyRange 0 s' ks
                    lvs = A.unsafeCopyRange 0 s' vs
                    rks = A.unsafeInsertIn s' rs (i - s') k ks
                    rvs = A.unsafeInsertIn s' rs (i - s') v vs
                    l = Leaf s' lks lvs
                    r = Leaf (rs + 1) rks rvs
                in Split l r
          where
            s' = s `div` 2
            rs = s - s'

    -- Insertion in a parent node
    insert' (Node s tv ks cs) = binarySearchWith found notFound s k ks
      where
        -- Found: we continue in the right child child. We also know the size
        -- cannot change (since no new key is added).
        found i = case insert' (A.unsafeIndex cs (i + 1)) of
            Ok c' -> Ok $ Node s tv ks (A.unsafePut (s + 1) (i + 1) c' cs)
            _     -> error "Data.BTree.insert: internal error!"

        -- Not found: left child. Now, it is possible that we have to split our
        -- node in order to balance the tree
        -- TODO: update size!
        notFound i = case insert' (A.unsafeIndex cs i) of
            Ok c' -> Ok $ Node s tv ks (A.unsafePut (s + 1) i c' cs)
            Split l r
                -- We're still good
                | s + 2 <= maxNodeSize ->
                    let -- Key to copy
                        !k' = A.unsafeIndex (nodeKeys r) 0
                        !ks' = A.unsafeInsert s i k' ks
                        !cs' = A.unsafePutPair (s + 1) i l r cs
                    in Ok $ Node (s + 1) tv ks' cs'
                -- We need to split this node. This should not happen often.
                -- TODO: This implementation can be written using at least one
                -- less copy.
                | otherwise ->
                    let -- Create a "too large" node
                        !k' = A.unsafeIndex (nodeKeys r) 0
                        !ks' = A.unsafeInsert s i k' ks
                        !cs' = A.unsafePutPair (s + 1) i l r cs
                        -- Currently: number of keys: s + 1, and s + 2 children
                        -- s + 1 is even, we will have one less key on the right
                        !ls = (s + 1) `div` 2
                        !rs = ls - 1
                        -- Select the left part
                        !lks = A.unsafeCopyRange 0 ls ks'
                        !lcs = A.unsafeCopyRange 0 (ls + 1) cs'
                        l' = Node ls tv lks lcs
                        -- Select the right part
                        !rks = A.unsafeCopyRange (ls + 1) rs ks'
                        !rcs = A.unsafeCopyRange (ls + 1) (rs + 1) cs'
                        r' = Node rs tv rks rcs
                    in Split l' r'
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

    showChild b i = map ("    " ++) $ showBTree' $
        A.unsafeIndex (nodeChildren b) i

    showKey b i = show $ A.unsafeIndex (nodeKeys b) i

    showTuple b i = show
        (A.unsafeIndex (nodeKeys b) i, A.unsafeIndex (nodeValues b) i)

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
    , minimumKey

      -- * Insertion
    , insert
    , insertWith

      -- * Debugging
    , showBTree
    ) where

import Prelude hiding (lookup)

-- import qualified Prelude as P

import Data.List (foldl')

import Data.BTree.Array.Search
import Data.BTree.Internal
import qualified Data.BTree.Array as A
import qualified Data.BTree.Array.Util as A

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
    lookup' (Leaf s ks vs)   = fmap (A.unsafeIndex vs) (search s k ks)
    lookup' (Node s _ ks cs) = searchWith found notFound s k ks
      where
        found i    = lookup' (A.unsafeIndex cs (i + 1))
        {-# INLINE found #-}
        notFound i = lookup' (A.unsafeIndex cs i)
        {-# INLINE notFound #-}
{-# INLINE lookup #-}

-- | Find the minimum key in a 'BTree'
minimumKey :: Ord k => BTree k v -> k
minimumKey (Leaf _ ks _)   = A.unsafeIndex ks 0
minimumKey (Node _ _ _ cs) = minimumKey (A.unsafeIndex cs 0)

-- | Signals for insertion
data Insert k v = Ok !(BTree k v)
                | Split !(BTree k v) !(BTree k v)

-- | Insert an element into the 'BTree'
insert :: Ord k => k -> v -> BTree k v -> BTree k v
insert = insertWith const
{-# INLINE insert #-}

-- | Insert an element into the 'BTree'
insertWith :: Ord k => (v -> v -> v) -> k -> v -> BTree k v -> BTree k v
insertWith f k v btree =
    -- Insertion in the root
    case insert' btree of
        Ok btree' -> btree'
        -- Split the root
        Split l r ->
            let !s = 1
                !tv = size l + size r
                !ks = A.singleton (minimumKey r)
                !cs = A.pair l r
            in Node s tv ks cs
  where
    -- Insertion in a leaf node
    insert' (Leaf s ks vs) = searchWith found notFound s k ks
      where
        -- Overwrite the value
        found i =
            let ov = A.unsafeIndex vs i  -- Do not force ov!
            in Ok $ Leaf s ks (A.unsafePut s i (f ov v) vs)

        -- Insert the value
        notFound i
            -- We have enough place, so just insert it
            | s + 1 <= maxNodeSize = Ok $ Leaf
                (s + 1) (A.unsafeInsert s i k ks) (A.unsafeInsert s i v vs)
            -- We need to split this leaf and insert left
            | i < s' =
                let lks = A.unsafeInsertIn 0 s' i k ks
                    lvs = A.unsafeInsertIn 0 s' i v vs
                    rks = A.unsafeCopyRange s' s' ks
                    rvs = A.unsafeCopyRange s' s' vs
                    l = Leaf (s' + 1) lks lvs
                    r = Leaf s' rks rvs
                in Split l r
            -- We need to split this leaf and insert right
            | otherwise =
                let lks = A.unsafeCopyRange 0 s' ks
                    lvs = A.unsafeCopyRange 0 s' vs
                    rks = A.unsafeInsertIn s' s' (i - s') k ks
                    rvs = A.unsafeInsertIn s' s' (i - s') v vs
                    l = Leaf s' lks lvs
                    r = Leaf (s' + 1) rks rvs
                in Split l r
          where
            s' = s `div` 2

    -- Insertion in a parent node
    insert' (Node s tv ks cs) = searchWith found notFound s k ks
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
                | s + 1 <= maxNodeSize ->
                    let -- Key to copy
                        !k' = minimumKey r
                        !ks' = A.unsafeInsert s i k' ks
                        !cs' = A.unsafePutPair (s + 1) i l r cs
                    in Ok $ Node (s + 1) tv ks' cs'
                -- We need to split this node. This should not happen often.
                -- TODO: This implementation can be written using at least one
                -- less copy.
                | otherwise ->
                    let -- Create a "too large" node
                        !k' = minimumKey r
                        !ks' = A.unsafeInsert s i k' ks
                        !cs' = A.unsafePutPair (s + 1) i l r cs
                        -- Currently: number of keys: s + 1, and s + 2 children
                        -- s + 1 is odd, so we can drop the key in the middle
                        !s' = s `div` 2
                        -- Select the left part
                        !lks = A.unsafeCopyRange 0 s' ks'
                        !lcs = A.unsafeCopyRange 0 (s' + 1) cs'
                        l' = Node s' tv lks lcs
                        -- Select the right part
                        !rks = A.unsafeCopyRange (s' + 1) s' ks'
                        !rcs = A.unsafeCopyRange (s' + 1) (s' + 1) cs'
                        r' = Node s' tv rks rcs
                    in Split l' r'
{-# INLINE insertWith #-}

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
    {-
    , minimumKey

      -- * Insertion
    , insert

      -- * Debugging
    , showBTree
    -}
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
empty = Node 0 A.empty A.empty N
{-# INLINE empty #-}

-- | Create a 'BTree' holding a single element
singleton :: Ord k => k -> v -> BTree k v
singleton k v = Node 1 (A.singleton k) (A.singleton v) N
{-# INLINE singleton #-}

-- | Create a 'BTree' from an associative list
fromList :: Ord k => [(k, v)] -> BTree k v
fromList = foldl' insert' empty
  where
    insert' t (k, v) = insert k v t

-- | Find the number of values in the 'BTree'
size :: BTree k v -> Int
size (Node s _ _ _) = s

-- | Find an element in the 'BTree'
lookup :: Ord k => k -> BTree k v -> Maybe v
lookup k = lookup'
  where
    lookup' (Node s ks vs cs) = searchWith found notFound s k ks
      where
        found i = Just (A.unsafeIndex vs i)
        {-# INLINE found #-}
        notFound i = case cs of
            N     -> Nothing
            J cs' -> lookup' (A.unsafeIndex cs' i)
        {-# INLINE notFound #-}
{-# INLINE lookup #-}

-- | Signals for insertion
data Insert k v = Ok !(BTree k v)
                -- Key, value to move up, left & right subtrees
                | Split !k !v !(BTree k v) !(BTree k v)

-- | Insert an element into the 'BTree'
insert :: Ord k => k -> v -> BTree k v -> BTree k v
insert k v btree =
    -- Insertion in the root
    case insert' btree of
        Ok btree' -> btree'
        -- Split the root
        Split k' v' l r ->
            let !ks = A.singleton k'
                !vs = A.singleton v'
                !cs = A.pair l r
            in Node 1 ks vs (J cs)
  where
    -- Insertion in a leaf node
    insert' (Node s ks vs N) = searchWith found notFound s k ks
      where
        -- Overwrite the value
        found i = Ok $ Node s ks (A.unsafePut s i v vs) N

        -- Insert the value
        notFound i
            -- We have enough place, so just insert it
            | s + 1 <= maxNodeSize = Ok $ Node
                (s + 1) (A.unsafeInsert s i k ks) (A.unsafeInsert s i v vs) N
            -- We first create a node which is "too large"
            | otherwise =
                let -- Too large keys, values
                    !ks' = A.unsafeInsert s i k ks
                    !vs' = A.unsafeInsert s i v vs
                    -- Key, value to move up
                    !k' = A.unsafeIndex ks' s'
                    !v' = A.unsafeIndex vs' s'
                    -- Left subtree
                    !lks = A.unsafeCopyRange 0 s' ks'
                    !lvs = A.unsafeCopyRange 0 s' vs'
                    !l = Node s' lks lvs N
                    -- Right subtree
                    !rks = A.unsafeCopyRange (s' + 1) s' ks'
                    !rvs = A.unsafeCopyRange (s' + 1) s' vs'
                    !r = Node s' rks rvs N
                in Split k' v' l r
          where
            s' = s `div` 2

    -- Insertion in a parent node
    insert' (Node s ks vs (J cs)) = searchWith found notFound s k ks
      where
        -- Found: overwrite
        found i = Ok $ Node s ks (A.unsafePut s i v vs) (J cs)

        -- Not found: left child. Now, it is possible that we have to split our
        -- node in order to balance the tree
        -- TODO: update size!
        notFound i = case insert' (A.unsafeIndex cs i) of
            Ok c' -> Ok $ Node s ks vs (J (A.unsafePut (s + 1) i c' cs))
            Split k' v' l r
                -- We're still good
                | s + 1 <= maxNodeSize ->
                    let !ks' = A.unsafeInsert s i k' ks
                        !vs' = A.unsafeInsert s i v' vs
                        !cs' = A.unsafePutPair (s + 1) i l r cs
                    in Ok $ Node (s + 1) ks' vs' (J cs')
                -- We need to split this node. This should not happen often.
                -- TODO: This implementation can be written using at least one
                -- less copy.
                | otherwise ->
                    let -- Create a "too large" node
                        !ks' = A.unsafeInsert s i k' ks
                        !vs' = A.unsafeInsert s i v' vs
                        !cs' = A.unsafePutPair (s + 1) i l r cs
                        -- Currently: number of keys: s + 1, and s + 2 children
                        -- s + 1 is odd, so we can move the key in the middle
                        !s' = s `div` 2
                        -- Key, value to move up
                        !k'' = A.unsafeIndex ks' s'
                        !v'' = A.unsafeIndex vs' s'
                        -- Select the left part
                        !lks = A.unsafeCopyRange 0 s' ks'
                        !lvs = A.unsafeCopyRange 0 s' vs'
                        !lcs = A.unsafeCopyRange 0 (s' + 1) cs'
                        l' = Node s' lks lvs (J lcs)
                        -- Select the right part
                        !rks = A.unsafeCopyRange (s' + 1) s' ks'
                        !rvs = A.unsafeCopyRange (s' + 1) s' vs'
                        !rcs = A.unsafeCopyRange (s' + 1) (s' + 1) cs'
                        r' = Node s' rks rvs (J rcs)
                    in Split k'' v'' l' r'
{-# INLINE insert #-}

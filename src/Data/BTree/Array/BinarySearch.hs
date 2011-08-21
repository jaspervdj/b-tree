-- | Auxiliary binary search functions for the implementation of the B-tree
{-# LANGUAGE BangPatterns #-}
module Data.BTree.Array.BinarySearch
    ( binarySearch
    , binarySearchWith
    ) where

import Data.BTree.Array (Array)
import qualified Data.BTree.Array as A

binarySearch :: Ord a
             => Int         -- ^ Size of the array to search
             -> a           -- ^ Item to search
             -> Array a     -- ^ Array to search
             -> Maybe Int   -- ^ Result
binarySearch size x arr = go 0 (size - 1)
  where
    go !lo !hi
        | lo > hi   = Nothing
        | otherwise =
            let !guess = (lo + hi) `div` 2
                !y     = A.unsafeIndex arr guess
            in case compare x y of
                LT -> go lo (guess - 1)
                GT -> go (guess + 1) hi
                EQ -> Just guess
{-# INLINE binarySearch #-}

binarySearchWith :: Ord a
                 => (Int -> b)  -- ^ Found; pass the index
                 -> (Int -> b)  -- ^ Not found; pass the first higher index
                 -> Int         -- ^ Size of the array to search
                 -> a           -- ^ Item to search
                 -> Array a     -- ^ Array to search
                 -> b           -- ^ Result
binarySearchWith found notFound size x arr = go 0 size
  where
    go !lo !hi
        | lo + 1 >= hi =
            if lo >= size
                then notFound lo
                else let !y = A.unsafeIndex arr lo
                     in case compare x y of
                         EQ -> found lo
                         LT -> notFound lo
                         GT -> notFound (lo + 1)
        | otherwise    =
            let !guess = (lo + hi) `div` 2
                !y     = A.unsafeIndex arr guess
            in case compare x y of
                LT -> go lo guess
                GT -> go guess hi
                EQ -> found guess
{-# INLINE binarySearchWith #-}

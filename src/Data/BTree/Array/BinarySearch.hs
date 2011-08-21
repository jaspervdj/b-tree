-- | Auxiliary binary search functions for the implementation of the B-tree
{-# LANGUAGE BangPatterns #-}
module Data.BTree.Array.BinarySearch
    ( binarySearch
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
    go !lo !up
        | lo > up   = Nothing
        | otherwise =
            let !guess = (lo + up) `div` 2
                !y     = A.unsafeIndex arr guess
            in case compare x y of
                LT -> go lo (guess - 1)
                GT -> go (guess + 1) up
                EQ -> Just guess
{-# INLINE binarySearch #-}

-- | Auxiliary search functions for the implementation of the B-tree
{-# LANGUAGE BangPatterns #-}
module Data.BTree.Array.Search
    ( search
    , searchWith
    ) where

import Data.BTree.Array (Array)
import qualified Data.BTree.Array as A

search :: Ord a
       => Int         -- ^ Size of the array to search
       -> a           -- ^ Item to search
       -> Array a     -- ^ Array to search
       -> Maybe Int   -- ^ Result
search size x arr = go 0
  where
    go !i
        | i >= size                = Nothing
        | A.unsafeIndex arr i == x = Just i
        | otherwise                = go (i + 1)
{-# INLINE search #-}

searchWith :: Ord a
           => (Int -> b)  -- ^ Found; pass the index
           -> (Int -> b)  -- ^ Not found; pass the first higher index
           -> Int         -- ^ Size of the array to search
           -> a           -- ^ Item to search
           -> Array a     -- ^ Array to search
           -> b           -- ^ Result
searchWith found notFound size x arr = go 0
  where
    go !i
        | i >= size = notFound i
        | otherwise = case compare (A.unsafeIndex arr i) x of
            LT -> go (i + 1)
            GT -> notFound i
            EQ -> found i
{-# INLINE searchWith #-}

-- | Functions to search through a single node in a 'BTree'
{-# LANGUAGE BangPatterns #-}
module Data.BTree.NodeSearch
    ( maxNodeSize
    , search
    , searchWith
    ) where

import Data.BTree.Array (Array)
import qualified Data.BTree.Array as A

-- | Maximum number of keys per node
maxNodeSize :: Int
maxNodeSize = 8
{-# INLINE maxNodeSize #-}

-- | Minimum size to tigger skipping
skipNodeSize :: Int
skipNodeSize = 7
{-# INLINE skipNodeSize #-}

-- | Skipping index
skipIndex :: Int
skipIndex = 3
{-# INLINE skipIndex #-}

search :: Ord a
       => Int         -- ^ Size of the array to search
       -> a           -- ^ Item to search
       -> Array a     -- ^ Array to search
       -> Maybe Int   -- ^ Result
search size x arr
    | size >= skipNodeSize =
        case compare (A.unsafeIndex arr skipIndex) x of
            GT -> go 0
            EQ -> Just skipIndex
            LT -> go (skipIndex + 1)
    | otherwise = go 0
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
searchWith found notFound size x arr
    | size >= skipNodeSize =
        case compare (A.unsafeIndex arr skipIndex) x of
            GT -> go 0
            EQ -> found skipIndex
            LT -> go (skipIndex + 1)
    | otherwise = go 0
  where
    go !i
        | i >= size = notFound i
        | otherwise = case compare (A.unsafeIndex arr i) x of
            LT -> go (i + 1)
            GT -> notFound i
            EQ -> found i
{-# INLINE searchWith #-}

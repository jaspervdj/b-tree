-- | Various utility functions for our array type
module Data.BTree.Array.Util
    ( empty
    , singleton
    , pair
    , unsafePut
    , unsafeInsert
    , unsafeInsertIn
    , unsafeCopyRange
    ) where

import Data.BTree.Array (Array)
import qualified Data.BTree.Array as A

-- | Create a new empty array
empty :: Array a
empty = A.run $ A.new 0
{-# INLINE empty #-}

-- | Create a new array holding a single element
singleton :: a -> Array a
singleton x = A.run $ A.new 1 >>= \mar -> A.unsafeWrite mar 0 x >> return mar
{-# INLINE singleton #-}

-- | Create a new array holding two elements
pair :: a -> a -> Array a
pair x y = A.run $ do
    mar <- A.new 2
    A.unsafeWrite mar 0 x
    A.unsafeWrite mar 1 y
    return mar
{-# INLINE pair #-}

-- | Set an element in the array. This creates a copy of the array.
unsafePut :: Int      -- ^ Size of the new array
          -> Int      -- ^ Index
          -> a        -- ^ Value
          -> Array a  -- ^ Array to modify
          -> Array a  -- ^ Modified array
unsafePut s i x ar = A.run $ do
    mar <- A.new s
    A.unsafeCopy ar 0 mar 0 s
    A.unsafeWrite mar i x
    return mar
{-# INLINE unsafePut #-}

-- | Insert an element in the array, shifting the values right of the index. The
-- array size should be big enough for this shift, this is not checked. This
-- creates a copy of the array.
unsafeInsert :: Int      -- ^ Size of the original array
             -> Int      -- ^ Index
             -> a        -- ^ Value
             -> Array a  -- ^ Array to modify
             -> Array a  -- ^ Modified array
unsafeInsert s i x ar = A.run $ do
    mar <- A.new (s + 1)
    -- TODO: Maybe check indices so we don't make too many calls to memcmp
    A.unsafeCopy ar 0 mar 0 i
    A.unsafeCopy ar i mar (i + 1) (s - i)
    A.unsafeWrite mar i x
    return mar
{-# INLINE unsafeInsert #-}

-- | Select a range from the array, and insert an element on a given position in
-- this range. Elements in the range right of the inserted element will be
-- shifted to the right. This creates a copy of the range.
unsafeInsertIn :: Int      -- ^ Range start index
               -> Int      -- ^ Range size
               -> Int      -- ^ Index in the range
               -> a        -- ^ Value
               -> Array a  -- ^ Array to modify
               -> Array a  -- ^ Modified array
unsafeInsertIn ri rs i x ar = A.run $ do
    mar <- A.new (rs + 1)
    let !i' = ri + i
    -- TODO: Maybe check indices so we don't make too many calls to memcmp
    A.unsafeCopy ar ri mar 0 i
    A.unsafeCopy ar i' mar (i + 1) (rs - i)
    A.unsafeWrite mar i x
    return mar
{-# INLINE unsafeInsertIn #-}

-- | Copy a part of an array into a new array. This creates a copy of the range.
unsafeCopyRange :: Int      -- ^ Start index
                -> Int      -- ^ Length
                -> Array a  -- ^ Array to copy
                -> Array a  -- ^ Resulting array
unsafeCopyRange i n ar = A.run $ do
    mar <- A.new n
    A.unsafeCopy ar i mar 0 n
    return mar
{-# INLINE unsafeCopyRange #-}

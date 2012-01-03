-- | Internal array type used by the B-tree types
{-# LANGUAGE BangPatterns, CPP, MagicHash, Rank2Types, UnboxedTuples #-}
module Data.BTree.Array
    ( -- * Types
      Array
    , MArray
    
      -- * Creating arrays
    , run
    , new
    , unsafeWrite
    , unsafeCopy

      -- * Inspecting arrays
    , unsafeIndex

      -- * Debugging
    , fromList
    , toList
    ) where

import GHC.ST (ST (..), runST)
import GHC.Exts

-- | Frozen array
data Array a = Array (Array# a)

-- | Mutable array
data MArray s a = MArray (MutableArray# s a)

-- | Create and freeze an array
run :: (forall s. ST s (MArray s a)) -> Array a
run act = runST $ act >>= unsafeFreeze
{-# INLINE run #-}

-- | Create a new array of the specified size
new :: Int -> ST s (MArray s a)
new (I# n#) = ST $ \s# -> case newArray# n# element s# of
    (# s'#, mar# #) -> (# s'#, MArray mar# #)
  where
    element = error "Data.BTree.Array: unitialized element!"
{-# INLINE new #-}

-- | Freeze a mutable array
unsafeFreeze :: MArray s a -> ST s (Array a)
unsafeFreeze (MArray mar#) = ST $ \s# ->
    case unsafeFreezeArray# mar# s# of (# s'#, ar# #) -> (# s'#, Array ar# #)
{-# INLINE unsafeFreeze #-}

-- | Write to a position in an array
unsafeWrite :: MArray s a -> Int -> a -> ST s ()
unsafeWrite (MArray mar#) (I# i#) x = ST $ \s# ->
    case writeArray# mar# i# x s# of s'# -> (# s'#, () #)
{-# INLINE unsafeWrite #-}

-- | Copy a range from an array
unsafeCopy :: Array a     -- ^ Array to copy from
           -> Int         -- ^ Source index
           -> MArray s a  -- ^ Destination to copy into
           -> Int         -- ^ Destination index
           -> Int         -- ^ Number of elements to copy
           -> ST s ()     -- ^ No result
-- TODO: Check this!
#if __GLASGOW_HASKELL__ >= 702
unsafeCopy (Array ar#) (I# si#) (MArray mar#) (I# di#) (I# n#) = ST $ \s# ->
    case copyArray# ar# si# mar# di# n# s# of s'# -> (# s'#, () #)
#else
unsafeCopy ar si mar di n = go si di 0
  where
    go !i !j !c
        | c >= n    = return ()
        | otherwise = do
            let x = unsafeIndex ar i
            unsafeWrite mar j x
            go (i + 1) (j + 1) (c + 1)
#endif
{-# INLINE unsafeCopy #-}

-- | Index an array, bounds are not checked
unsafeIndex :: Array a -> Int -> a
unsafeIndex (Array ar#) (I# i#)
    | i# <# 0#  = error "herp"
    | otherwise = case indexArray# ar# i# of (# x #) -> x
{-# INLINE unsafeIndex #-}

-- | Convert a list to an array. For debugging purposes only.
fromList :: [a] -> Array a
fromList ls = run $ do
    mar <- new (length ls)
    let go _ []       = return mar
        go i (x : xs) = do
            unsafeWrite mar i x
            go (i + 1) xs
    go 0 ls

-- | Convert an array to a list. For debugging purposes only.
toList :: Int      -- ^ Length
       -> Array a  -- ^ Array to convert to a list
       -> [a]      -- ^ Resulting list
toList n arr = go 0
  where
    go !i | i >= n    = []
          | otherwise = unsafeIndex arr i : go (i + 1)

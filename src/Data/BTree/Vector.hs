-- | This module contains auxiliary 'V.MVector' functions for the implementation
-- of the B-tree.
{-# LANGUAGE BangPatterns #-}
module Data.BTree.Vector
    ( unsafeShiftL
    , unsafeShiftR
    ) where

import Control.Monad.ST (ST)
import qualified Data.Vector.Mutable as V

-- | Shift part of a vector left by one
unsafeShiftL :: Int            -- ^ Start of the range
             -> Int            -- ^ Index /after/ the range
             -> V.MVector s a  -- ^ Vector of which a part should be shifted
             -> ST s ()
unsafeShiftL lo up vector = unsafeShiftL' lo
  where
    unsafeShiftL' !i
        | i >= up   = return ()
        | otherwise = do
            x <- V.unsafeRead vector i
            V.unsafeWrite vector (i - 1) x
            unsafeShiftL' (i + 1)
{-# INLINE [0] unsafeShiftL #-}

-- | Shift part of a vector left by one
unsafeShiftR :: Int            -- ^ Start of the range
             -> Int            -- ^ Index /after/ the range
             -> V.MVector s a  -- ^ Vector of which a part should be shifted
             -> ST s ()
unsafeShiftR lo up vector = unsafeShiftR' up
  where
    unsafeShiftR' !i
        | i <= lo   = return ()
        | otherwise = do
            let !i' = i - 1
            x <- V.unsafeRead vector i'
            V.unsafeWrite vector i x
            unsafeShiftR' i'
{-# INLINE [0] unsafeShiftR #-}

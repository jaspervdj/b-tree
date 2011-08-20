-- | This module contains auxiliary 'V.MVector' functions for the implementation
-- of the B-tree.
{-# LANGUAGE BangPatterns #-}
module Data.BTree.Vector
    ( shiftLeft
    , shiftRight
    ) where

import Control.Monad.ST (ST)
import qualified Data.Vector.Mutable as V

-- | Shift part of a vector left by one
shiftLeft :: Int            -- ^ Start of the range
          -> Int            -- ^ Index /after/ the range
          -> V.MVector s a  -- ^ Vector of which a part should be shifted
          -> ST s ()
shiftLeft lo up vector = shiftLeft' lo
  where
    shiftLeft' !i
        | i >= up   = return ()
        | otherwise = do
            x <- V.read vector i
            V.write vector (i - 1) x
            shiftLeft' (i + 1)
{-# INLINE [0] shiftLeft #-}

-- | Shift part of a vector left by one
shiftRight :: Int            -- ^ Start of the range
           -> Int            -- ^ Index /after/ the range
           -> V.MVector s a  -- ^ Vector of which a part should be shifted
           -> ST s ()
shiftRight lo up vector = shiftRight' up
  where
    shiftRight' !i
        | i <= lo   = return ()
        | otherwise = do
            let !i' = i - 1
            x <- V.read vector i'
            V.write vector i x
            shiftRight' i'
{-# INLINE [0] shiftRight #-}

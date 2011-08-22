{-# LANGUAGE BangPatterns, PatternGuards #-}
module Data.BTree.Internal
    ( BTree (..)
    , maxNodeSize
    , showBTree
    ) where

import qualified Data.BTree.Array as A

data BTree k v
    = Node
        { nodeSize        :: !Int
        , nodeTotalValues :: !Int
        , nodeKeys        :: !(A.Array k)
        , nodeChildren    :: !(A.Array (BTree k v))
        }
    | Leaf
        { nodeSize      :: !Int
        , nodeKeys      :: !(A.Array k)
        , nodeValues    :: !(A.Array v)
        }

maxNodeSize :: Int
maxNodeSize = 4
{-# INLINE maxNodeSize #-}

-- | Show the internal structure of a 'BTree', useful for debugging
showBTree :: (Show k, Show v) => BTree k v -> String
showBTree btree = unlines $ showBTree' btree
  where
    showBTree' b = case b of
        Node s _ _ _ -> concatMap (showElement b) [0 .. s]
        Leaf s _ _   -> map (showTuple b) [0 .. s - 1]

    showElement b i
        | i == nodeSize b = showChild b i
        | otherwise       = showChild b i ++ [showKey b i]

    showChild b i = map ("    " ++) $ showBTree' $
        A.unsafeIndex (nodeChildren b) i

    showKey b i = show $ A.unsafeIndex (nodeKeys b) i

    showTuple b i = show
        (A.unsafeIndex (nodeKeys b) i, A.unsafeIndex (nodeValues b) i)

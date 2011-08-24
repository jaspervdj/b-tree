{-# LANGUAGE BangPatterns, PatternGuards #-}
module Data.BTree.Internal
    ( M (..)
    , BTree (..)
    , maxNodeSize
    , showBTree
    ) where

import qualified Data.BTree.Array as A

-- | Strict maybe type
data M a = J {-# UNPACK #-} !a | N

data BTree k v
    = Node
        { nodeSize        :: {-# UNPACK #-} !Int
        , nodeKeys        :: {-# UNPACK #-} !(A.Array k)
        , nodeValues      :: {-# UNPACK #-} !(A.Array v)
        , nodeChildren    :: {-# UNPACK #-} !(M (A.Array (BTree k v)))
        }

-- | Maximum number of keys per node
maxNodeSize :: Int
maxNodeSize = 8
{-# INLINE maxNodeSize #-}

-- | Show the internal structure of a 'BTree', useful for debugging
showBTree :: (Show k, Show v) => BTree k v -> String
showBTree = unlines . showBTree'
  where
    showBTree' b = concatMap (showElement b) [0 .. nodeSize b]

    showElement b i
        | i == nodeSize b = showChild b i
        | otherwise       = showChild b i ++ [showTuple b i]

    showChild b i = case nodeChildren b of
        N   -> []
        J c -> map ("    " ++) $ showBTree' $ A.unsafeIndex c i

    showTuple b i = show
        (A.unsafeIndex (nodeKeys b) i, A.unsafeIndex (nodeValues b) i)

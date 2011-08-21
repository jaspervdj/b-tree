{-# LANGUAGE BangPatterns #-}
module Data.BTree
    ( -- * Types
      BTree

      -- * Creation
    , empty
    , singleton

      -- * Inspecting
    , lookup

      -- * Debugging
    , showBTree
    ) where

import Prelude hiding (lookup)

import Control.Applicative ((<$>), (<*>))
import Control.Monad.ST (ST, runST)
import Control.Monad (liftM3, mapM, sequence)
import Data.List (intercalate)
import qualified Prelude as P
import qualified Data.Vector.Mutable as V

import Data.BTree.Array.BinarySearch
import qualified Data.BTree.Array as A

childrenPerNode :: Int
childrenPerNode = 16
{-# INLINE childrenPerNode #-}

data BTree k v
    = Node
        { nodeSize      :: !Int
        , nodeTotalSize :: !Int
        , nodeKeys      :: !(A.Array k)
        , nodeChildren  :: !(A.Array (BTree k v))
        }
    | Leaf
        { nodeSize      :: !Int
        , nodeKeys      :: !(A.Array k)
        , nodeValues    :: !(A.Array v)
        }

-- | Create an empty 'BTree'
empty :: Ord k => BTree k v
empty = Leaf 0 A.empty A.empty
{-# INLINE empty #-}

-- | Create a 'BTree' holding a single element
singleton :: Ord k => k -> v -> BTree k v
singleton k v = Leaf 1 (A.singleton k) (A.singleton v)
{-# INLINE singleton #-}

-- | Find an element in the 'BTree'
lookup :: Ord k => k -> BTree k v -> Maybe v
lookup x (Leaf s k v) = fmap (A.unsafeIndex v) (binarySearch s x k)
{-# INLINE lookup #-}

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

    showChild b i = map (++ "    ") $ showBTree' $
        A.unsafeIndex (nodeChildren b) i

    showKey b i = show $ A.unsafeIndex (nodeKeys b) i

    showTuple b i = show
        (A.unsafeIndex (nodeKeys b) i, A.unsafeIndex (nodeValues b) i)

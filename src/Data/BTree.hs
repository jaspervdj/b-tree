{-# LANGUAGE BangPatterns #-}
module Data.BTree where

import Prelude ( Bool, Int, Monad, Ord, Ordering (..), Show, div, compare
               , otherwise, return, undefined, (+), (-), (/), (.), ($), (==)
               , (<=), (>=)
               )

import Control.Applicative ((<$>), (<*>))
import Control.Monad.ST (ST)
import Control.Monad (liftM3)
import qualified Prelude as P
import qualified Data.Vector.Mutable as V

childrenPerNode :: Int
childrenPerNode = 24

data BTree s k v = BTree
    { nodeSize     :: !Int
    , nodeKeys     :: !(V.STVector s k)
    , nodeValues   :: !(V.STVector s v)
    , nodeChildren :: !(V.STVector s (BTree s k v))
    }

-- | Check whether or not a certain 'BTree' does not have any children
hasChildren :: BTree s k v -> Bool
hasChildren btree = nodeSize btree >= childrenPerNode
{-# INLINE [0] hasChildren #-}

size :: BTree s k v -> Int
size (BTree s _ _ _) = s

new :: ST s (BTree s k v)
new = do
    keys     <- V.new (childrenPerNode - 1)
    values   <- V.new (childrenPerNode - 1)
    children <- V.new childrenPerNode

    return $ BTree 0 keys values children
{-# INLINE [0] new #-}

singleton :: k -> v -> ST s (BTree s k v)
singleton k v = do
    keys     <- V.new (childrenPerNode - 1)
    values   <- V.new (childrenPerNode - 1)
    children <- V.new childrenPerNode

    V.write keys   0 k
    V.write values 0 v

    return $ BTree 1 keys values children
{-# INLINE [0] singleton #-}

insert :: Ord k => k -> v -> BTree s k v -> ST s (BTree s k v)
insert k v (BTree s keys values children) = findIndex 0 s
  where
    findIndex !lo !hi
        | lo == hi = do
            -- child  <- V.read children lo 
            -- child' <- insertInChild k v child
            -- V.write children lo child'
            undefined
        | lo == s  = undefined -- insertInChild (size - 1)
        | otherwise  = do
            let !i = (lo + hi) `div` 2
            x <- V.read keys i
            case compare k x of
                EQ -> undefined
                LT -> findIndex lo i
                GT -> findIndex i hi

-- insertInChild :: Ord k => k -> v -> BTree s k v -> ST s (BTree s k v)
-- insertInChild k v Null = singleton k v
-- insertInChild k v t    = insert k v t

{-
new :: PrimMonad m => m (BTree (PrimState m) k v)
new = return Null

singleton :: PrimMonad m
          => k
          -> v
          -> m (BTree (PrimState m) k v)
singleton k v = do undefined

insert :: (PrimMonad m, Ord k)
       => k
       -> v
       -> (BTree (PrimState m) k v)
       -> m (BTree (PrimState m) k v)
insert k v Null = undefined
-}

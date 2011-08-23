module Data.BTree.Benchmarks
    ( benchmarks
    ) where

import Criterion (Benchmark, bench, bgroup, nf, whnf)

import qualified Data.BTree as B
import qualified Data.Map as M

benchmarks :: Benchmark
benchmarks = bgroup "Data.BTree.Benchmarks"
    [ bench "BTree.fromList" $ whnf B.fromList assocList
    , bench "Map.fromList"   $ whnf M.fromList assocList

    , bench "BTree.lookup" $
        nf (\t -> map (flip B.lookup t) lookupKeys) (B.fromList assocList)
    , bench "Map.lookup"   $
        nf (\m -> map (flip M.lookup m) lookupKeys) (M.fromList assocList)
    ]

-- | Some data to build a map of
assocList :: [(Int, Int)]
assocList = zip keys values
  where
    keys = [1, 3 .. 999] ++ [2000, 1999 .. 1000] ++ [1, 5 .. 2000]
    values = [1 ..]
{-# NOINLINE assocList #-}

-- | Some keys to look up
lookupKeys :: [Int]
lookupKeys = [1, 4 .. 2000]

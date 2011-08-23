module Data.BTree.Benchmarks
    ( benchmarks
    ) where

import Criterion (Benchmark, bench, bgroup, nf)

benchmarks :: Benchmark
benchmarks = bgroup "Data.BTree.Benchmarks"
    [ bench "None" $ nf (+ 3) (2 :: Int)
    ]

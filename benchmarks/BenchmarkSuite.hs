module BenchmarkSuite
    ( main
    ) where

import Criterion.Main (defaultMain)

import qualified Data.BTree.Benchmarks as Data.BTree.Benchmarks

main :: IO ()
main = defaultMain
    [ Data.BTree.Benchmarks.benchmarks
    ]

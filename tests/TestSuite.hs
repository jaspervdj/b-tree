module TestSuite
    ( main
    ) where

import Test.Framework (defaultMain)

import qualified Data.BTree.Array.BinarySearch.Tests as Array.BinarySearch.Tests
import qualified Data.BTree.Tests as Tests

main :: IO ()
main = defaultMain
    [ Array.BinarySearch.Tests.tests
    , Tests.tests
    ]

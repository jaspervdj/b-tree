module TestSuite
    ( main
    ) where

import Test.Framework (defaultMain)

import qualified Data.BTree.Array.Search.Tests as Array.Search.Tests
import qualified Data.BTree.Tests as Tests

main :: IO ()
main = defaultMain
    [ Array.Search.Tests.tests
    , Tests.tests
    ]

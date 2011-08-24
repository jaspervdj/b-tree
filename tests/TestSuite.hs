module TestSuite
    ( main
    ) where

import Test.Framework (defaultMain)

import qualified Data.BTree.NodeSearch.Tests as NodeSearch.Tests
import qualified Data.BTree.Tests as Tests

main :: IO ()
main = defaultMain
    [ NodeSearch.Tests.tests
    , Tests.tests
    ]

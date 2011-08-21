import Test.Framework (defaultMain)

import qualified Data.BTree.Array.BinarySearch.Tests as Array.BinarySearch.Tests

main :: IO ()
main = defaultMain
    [ Array.BinarySearch.Tests.tests
    ]

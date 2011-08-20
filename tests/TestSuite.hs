import Test.Framework (defaultMain)

import qualified Data.BTree.Vector.Tests as Data.BTree.Vector.Tests

main :: IO ()
main = defaultMain
    [ Data.BTree.Vector.Tests.tests
    ]

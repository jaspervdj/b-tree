import Test.Framework (defaultMain)

import qualified Data.BTree.Array.Util.Tests as Data.BTree.Array.Util.Tests

main :: IO ()
main = defaultMain
    [ Data.BTree.Array.Util.Tests.tests
    ]

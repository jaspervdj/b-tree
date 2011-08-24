import Data.BTree
import Data.BTree.Internal
import Data.BTree.Invariants
import qualified Data.BTree as B

dump :: (Show k, Show v) => BTree k v -> IO ()
dump = putStr . showBTree

testData :: BTree Int String
testData = fromList
    [ (0,  "Zero")
    , (1,  "One")
    , (2,  "Two")
    , (3,  "Three")
    , (4,  "Four")
    , (5,  "Five")
    , (6,  "Six")
    , (7,  "Seven")
    , (8,  "Eight")
    , (9,  "Nine")
    , (10, "Ten")
    , (11, "Eleven")
    , (12, "Twelve")
    , (13, "Thirteen")
    , (14, "Fourteen")
    , (15, "Fifteen")
    , (16, "Sixteen")

    , (-1, "Minus one")
    , (-2, "Minus two")
    , (-3, "Minus three")
    , (-4, "Minus four")
    , (-5, "Minus five")
    ]

hard :: BTree Int Int
hard = fromList
    [(-5,0),(-6,0),(-7,0),(-3,0),(-8,0),(-4,0),(-9,0),(0,0),(2,0),(3,0),(-1,0),(1,0),(-10,0),(-2,0)]
{-# NOINLINE hard #-}

main :: IO ()
main = putStr $ show $ B.lookup 40 hard

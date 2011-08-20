module Data.BTree.Vector.Tests
    ( tests
    ) where

import Control.Monad (forM, forM_, replicateM)
import Control.Monad.ST (runST)

import qualified Data.Vector.Mutable as V
import Test.QuickCheck (Arbitrary (..), choose, elements)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Data.BTree.Vector

tests :: Test
tests = testGroup "Data.BTree.Vector.Tests"
    [ testProperty "shiftTest" shiftTest
    ]

data ShiftTestDirection = L | R
                        deriving (Eq, Show)

instance Arbitrary ShiftTestDirection where
    arbitrary = elements [L, R]

data ShiftTest = ShiftTest ShiftTestDirection Int Int [Int]
               deriving (Show)

instance Arbitrary ShiftTest where
    arbitrary = do
        dir <- arbitrary
        len <- choose (4, 128)
        lo  <- choose $ case dir of
                L -> (1, len)
                R -> (0, len - 1)
        hi  <- choose $ case dir of
                L -> (lo, len)
                R -> (lo, len - 1)
        ns  <- replicateM len arbitrary
        return $ ShiftTest dir lo hi ns

shiftTest :: ShiftTest -> Bool
shiftTest (ShiftTest dir lo hi ns) = expected == result
  where
    -- The expected result
    expected
        | lo == hi  = ns
        | otherwise = case dir of
            L -> take (lo - 1) before ++ moved ++ drop (hi - lo - 1) moved ++ after
            R -> before ++ take 1 moved ++ moved ++ drop 1 after
      where
        -- The moved part, and the parts before/after it
        (before, r)    = splitAt lo        ns
        (moved, after) = splitAt (hi - lo) r

    -- Actual run
    result = runST $ do
        -- Allocate vector and write data
        v <- V.new length'
        forM_ (zip [0 ..] ns) $ \(i, x) -> V.write v i x

        -- Shift
        (case dir of L -> shiftLeft; R -> shiftRight) lo hi v

        -- Read the vector again
        forM [0 .. length' - 1] $ V.read v
      where
        length' = length ns

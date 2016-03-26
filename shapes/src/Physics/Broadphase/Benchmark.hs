{-# LANGUAGE MagicHash #-}

module Physics.Broadphase.Benchmark where

import Criterion.Main

import qualified Physics.Broadphase.OptAabb as OB
import qualified Physics.Broadphase as B
import qualified Physics.Contact.OptConvexHull as OC
import qualified Physics.ConvexHull as C

import Utils.Utils

import Physics.Contact.Benchmark (testBoxes, testOptBoxes)

-- TODO: report this somehow? probably doesn't segfault in GHCI or without -O.
{-
{-# LANGUAGE MagicHash #-}

import GHC.Prim
import GHC.Types

data TestS a = TestS !a deriving Show
data TestInner = TestInner Double#

instance Show TestInner where
  show _ = "(TestInner _)"

testSegfault :: TestS Bool
testSegfault =
  TestS (testWeirdCompare a b)
  where a = TestInner 1.0##
        b = TestInner 2.0##

testWeirdCompare :: TestInner -> TestInner -> Bool
testWeirdCompare (TestInner x) (TestInner y) =
  isTrue# (notI# ((x >## y) `orI#` (y >## x)))
-}

testAabb :: C.ConvexHull Double
         -> C.ConvexHull Double
         -> SP (SP' (B.Aabb Double)) Bool
testAabb a b = SP (SP boxA boxB) (B.aabbCheck boxA boxB)
  where boxA = B.toAabb a
        boxB = B.toAabb b

testOptAabb :: OC.ConvexHull
            -> OC.ConvexHull
            -> SP (SP' OB.Aabb) Bool
testOptAabb a b = SP (SP boxA boxB) (OB.aabbCheck boxA boxB)
  where boxA = OB.toAabb a
        boxB = OB.toAabb b

benchy :: Benchmark
benchy = bench "aabb" $ whnf (uncurry testAabb) testBoxes

benchy' :: Benchmark
benchy' = bench "opt aabb" $ whnf (uncurry testOptAabb) testOptBoxes

main :: IO ()
main = do
  print . uncurry testAabb $ testBoxes
  print . uncurry testOptAabb $ testOptBoxes
  defaultMain [benchy, benchy']

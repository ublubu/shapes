{-# LANGUAGE MagicHash #-}

module Physics.Broadphase.Benchmark where

import Criterion.Main

import qualified Physics.Broadphase.Opt.Aabb as OB
import qualified Physics.Contact.Opt.ConvexHull as OC
import Physics.World.Opt
import Physics.World.Opt.Object

import Physics.Engine.Class (makeWorld)
import Physics.Engine.Opt (engineP)
import Physics.Scenes.Stacks

import Utils.Utils

import Physics.Contact.Benchmark (testOptBoxes)

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

testOptAabb :: OC.ConvexHull
            -> OC.ConvexHull
            -> SP (SP' OB.Aabb) Bool
testOptAabb a b = SP (SP boxA boxB) (OB.aabbCheck boxA boxB)
  where boxA = OB.toAabb a
        boxB = OB.toAabb b

testWorld :: World WorldObj
testWorld =
  makeWorld engineP $ stacks engineP (0.2, 0.2) (0, -4.5) (0, 0) 0 (30, 30)

benchy :: [Benchmark]
benchy = [ bench "opt aabb" $ whnf (uncurry testOptAabb) testOptBoxes
         , bench "opt broadphase" $ nf OB.culledKeys testWorld
         ]

main :: IO ()
main = do
  print . uncurry testOptAabb $ testOptBoxes
  defaultMain benchy

{-# LANGUAGE MagicHash #-}

module Physics.Broadphase.Benchmark where

import Criterion.Main
import Data.Proxy

import qualified Physics.Broadphase.Opt.Aabb as OB
import qualified Physics.Broadphase.Simple.Aabb as B
import qualified Physics.Contact.Opt.ConvexHull as OC
import qualified Physics.Contact.Simple.ConvexHull as C

import Physics.Engine.Class
import Physics.Engine.Opt (Engine)
import Physics.Engine.Simple (SimpleEngine)
import Physics.Scenes.Stacks

import Utils.Utils

import Physics.Contact.Benchmark (testBoxes, testOptBoxes)

simpleEngine :: Proxy SimpleEngine
simpleEngine = Proxy

optEngine :: Proxy Engine
optEngine = Proxy

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

testWorld :: (PhysicsEngine e) => Proxy e -> PEWorld' e
testWorld p =
  makeWorld p $ stacks p (0.2, 0.2) (0, -4.5) (0, 0) 0 (30, 30)

simpleWorld :: PEWorld' SimpleEngine
simpleWorld = testWorld simpleEngine

optWorld :: PEWorld' Engine
optWorld = testWorld optEngine

benchy :: [Benchmark]
benchy = [ bench "aabb" $ whnf (uncurry testAabb) testBoxes
         , bench "broadphase" $ nf B.culledKeys simpleWorld ]

benchy' :: [Benchmark]
benchy' = [ bench "opt aabb" $ whnf (uncurry testOptAabb) testOptBoxes
          , bench "opt broadphase" $ nf OB.culledKeys optWorld ]

main :: IO ()
main = do
  print . uncurry testAabb $ testBoxes
  print . uncurry testOptAabb $ testOptBoxes
  defaultMain $ benchy ++ benchy'

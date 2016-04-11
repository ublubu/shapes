{-# LANGUAGE MagicHash #-}

module Physics.Constraint.Benchmark where

import Criterion.Main

import Physics.Constraint.Opt
import Physics.Linear.Opt
import Utils.Utils

testConstraint :: Constraint
testConstraint = Constraint j 0
  where j = ja `join3v3` jb
        ja = negateV2 n `append2` ((xa `minusV2` p') `crossV2` n)
        jb = n `append2` ((p' `minusV2` xb) `crossV2` n)
        xa = V2 0.0## 0.0##
        xb = V2 0.0## 4.0##
        p' = V2 0.0## 2.0##
        n = V2 0.0## 1.0##

testObjPair :: (PhysicalObj, PhysicalObj)
testObjPair =
  ( PhysicalObj { _physObjVel = V2 1.0## 1.0##
                , _physObjRotVel = 0
                , _physObjPos = V2 0.0## 0.0##
                , _physObjRotPos = 0
                , _physObjInvMass = InvMass2 1.0## 2.0##
                }
  , PhysicalObj { _physObjVel = V2 1.0## (-1.0##)
                , _physObjRotVel = 0
                , _physObjPos = V2 0.0## 4.0##
                , _physObjRotPos = 0
                , _physObjInvMass = InvMass2 1.0## 2.0##
                }
  )

benchy :: Benchmark
benchy = bench "solveConstraint" $ whnf (toSP . uncurry solveConstraint) (testConstraint, testObjPair)

main :: IO ()
main = do
  print $ solveConstraint testConstraint testObjPair
  defaultMain [benchy]

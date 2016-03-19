{-# LANGUAGE MagicHash #-}

module Physics.Contact.Benchmark where

import GHC.Types (Double(D#))

import Criterion.Main
import Linear.V2

import qualified Physics.Constraint as C
import qualified Physics.Constraint.OptConstraint as OC
import Physics.Linear
import Utils.Utils

testConstraint :: C.Constraint Double
testConstraint = C.Constraint j 0
  where j = (ja `join33` jb)
        ja = (-n) `append2` ((xa - p') `cross22` n)
        jb = n `append2` ((p' - xb) `cross22` n)
        xa = V2 0 0
        xb = V2 0 4
        p' = V2 0 2
        n = V2 0 1

toOptConstraint :: C.Constraint Double -> OC.Constraint
toOptConstraint (C.Constraint j b) = OC.Constraint j b

testOptConstraint :: OC.Constraint
testOptConstraint = toOptConstraint testConstraint

testObjPair :: (C.PhysicalObj Double, C.PhysicalObj Double)
testObjPair = ( C.PhysicalObj { C._physObjVel = V2 1 1
                              , C._physObjRotVel = 0
                              , C._physObjPos = V2 0 0
                              , C._physObjRotPos = 0
                              , C._physObjInvMass = (1, 2) }
              , C.PhysicalObj { C._physObjVel = V2 1 (-1)
                              , C._physObjRotVel = 0
                              , C._physObjPos = V2 0 4
                              , C._physObjRotPos = 0
                              , C._physObjInvMass = (1, 2) } )

toOptInvMass :: C.InvMass2 Double -> OC.InvMass2
toOptInvMass (D# m, D# i) = OC.InvMass2 m i

toOptObj :: C.PhysicalObj Double -> OC.PhysicalObj
toOptObj (C.PhysicalObj a b c d e) = OC.PhysicalObj a b c d (toOptInvMass e)

testOptObjPair :: (OC.PhysicalObj, OC.PhysicalObj)
testOptObjPair = pairMap toOptObj testObjPair

benchy :: Benchmark
benchy = bench "solveConstraint" $ whnf (toSP . uncurry C.solveConstraint) (testConstraint, testObjPair)

benchOpty :: Benchmark
benchOpty = bench "opt solveConstraint" $ whnf (toSP . uncurry OC.solveConstraint) (testOptConstraint, testOptObjPair)

main :: IO ()
main = defaultMain [benchy, benchOpty]

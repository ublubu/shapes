{-# LANGUAGE RecordWildCards #-}

{- |
Generate and solve friction constraints for colliding objects.
-}
module Physics.Constraints.Contact.Friction where

import           Control.Lens

import           Physics.Constraint
import           Physics.Constraints.SolutionProcessors
import           Physics.Constraints.Types
import           Physics.Contact.Types
import           Physics.Linear

import           Utils.Utils

constraintGen :: Flipping Contact
              -> (PhysicalObj, PhysicalObj)
              -> Constraint
constraintGen fContact ab =
  flipExtract $ flipMap toConstraint fContact ab
{-# INLINE constraintGen #-}

toConstraint :: Contact
             -> (PhysicalObj, PhysicalObj)
             -> Constraint
toConstraint c ab = Constraint (jacobian c ab) 0
{-# INLINE toConstraint #-}

jacobian :: Contact
         -> (PhysicalObj, PhysicalObj)
         -> V6
jacobian Contact {..} (a, b) = ja `join3v3` jb
  where
    ja = ta `append2` ((p' `minusV2` xa) `crossV2` ta)
    jb = tb `append2` ((p' `minusV2` xb) `crossV2` tb)
    xa = _physObjPos a
    xb = _physObjPos b
    (P2 p') = _contactCenter
    ta = negateV2 tb
    tb = clockwiseV2 n
    n = _contactNormal
{-# INLINE jacobian #-}

pairMu :: (Double, Double) -> Double
pairMu (ua, ub) = (ua + ub) / 2
{-# INLINE pairMu #-}

solutionProcessor :: (Double, Double)
                  -> Lagrangian
                  -> Lagrangian
                  -> Lagrangian
                  -> Processed Lagrangian
solutionProcessor ab nonpen = clampAbs (nonpen & lagrangianVal *~ pairMu ab)
{-# INLINE solutionProcessor #-}

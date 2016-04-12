{-# LANGUAGE RecordWildCards #-}

module Physics.Constraints.Contact.Friction where

import Control.Lens

import Physics.Constraint
import Physics.Constraints.Types
import Physics.Constraints.SolutionProcessors
import Physics.Contact
import Physics.Linear

import Utils.Utils

constraintGen :: (Contactable a)
              => Flipping Contact'
              -> (a, a)
              -> Constraint
constraintGen fContact ab =
  flipExtract $ flipMap toConstraint fContact ab'
  where ab' = ab & each %~ view physObj
{-# INLINE constraintGen #-}

toConstraint :: Contact'
             -> (PhysicalObj, PhysicalObj)
             -> Constraint
toConstraint c ab = Constraint (jacobian c ab) 0
{-# INLINE toConstraint #-}

jacobian :: Contact'
         -> (PhysicalObj, PhysicalObj)
         -> V6
jacobian Contact'{..} (a, b) = ja `join3v3` jb
  where ja = ta `append2` ((p' `minusV2` xa) `crossV2` ta)
        jb = tb `append2` ((p' `minusV2` xb) `crossV2` tb)
        xa = _physObjPos a
        xb = _physObjPos b
        (P2 p') = _contactPenetrator'
        ta = negateV2 tb
        tb = clockwiseV2 n
        n = _contactEdgeNormal'
{-# INLINE jacobian #-}

pairMu :: (Contactable p) => (p, p) -> Double
pairMu ab = (ua + ub) / 2
  where (ua, ub) = pairMap contactMu ab
{-# INLINE pairMu #-}

solutionProcessor :: (Contactable a)
                  => (a, a)
                  -> Lagrangian
                  -> Lagrangian
                  -> Lagrangian
                  -> Processed Lagrangian
solutionProcessor ab nonpen = clampAbs (nonpen & lagrangianVal *~ pairMu ab)
{-# INLINE solutionProcessor #-}

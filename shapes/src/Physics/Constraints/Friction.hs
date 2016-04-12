{-# LANGUAGE RecordWildCards #-}

module Physics.Constraints.Friction where

import Physics.Constraint
import Physics.Linear
import Physics.Contact
import Utils.Utils

toConstraint :: ContactBehavior
             -> Double
             -> Contact'
             -> (PhysicalObj, PhysicalObj)
             -> Constraint
toConstraint _ _ c ab = Constraint (jacobian c ab) 0
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


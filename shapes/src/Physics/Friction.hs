{-# LANGUAGE RecordWildCards #-}

module Physics.Friction where

import Control.Lens
import Linear.Affine
import Physics.Constraint
import Physics.Contact
import Physics.ContactSolver
import Physics.ConvexHull
import Physics.Linear
import Utils.Utils

toConstraint :: (Fractional a, Ord a)
             => ContactBehavior a
             -> a
             -> Contact' a
             -> (PhysicalObj a, PhysicalObj a)
             -> Constraint a
toConstraint _ _ c ab = Constraint (jacobian c ab) 0

jacobian :: (Num a)
         => Contact' a
         -> (PhysicalObj a, PhysicalObj a)
         -> V6 a
jacobian Contact'{..} (a, b) = ja `join33` jb
  where ja = ta `append2` ((p' - xa) `cross22` ta)
        jb = tb `append2` ((p' - xb) `cross22` tb)
        xa = _physObjPos a
        xb = _physObjPos b
        p' = view _Point . _neighborhoodCenter $ _contactPenetrator'
        ta = -tb
        tb = clockwise2 n
        n = _neighborhoodUnitNormal _contactEdge'

pairMu :: (Contactable a p, Fractional a) => (p, p) -> a
pairMu ab = (ua + ub) / 2
  where (ua, ub) = pairMap contactMu ab


{-# LANGUAGE RecordWildCards #-}

module Physics.NonPenetration where

import Control.Lens
import Linear.Affine
import Physics.Constraint
import Physics.Contact
import Physics.ConvexHull
import Physics.Linear

toConstraint :: (Fractional a, Ord a)
             => ContactBehavior a
             -> a
             -> Contact' a
             -> (PhysicalObj a, PhysicalObj a)
             -> Constraint a
toConstraint beh dt c ab = Constraint (jacobian c ab) (baumgarte beh dt c)

-- TODO: comment or name stuff so it's clear that `a` is penetrated by `b`
jacobian :: (Num a)
         => Contact' a
         -> (PhysicalObj a, PhysicalObj a)
         -> V6 a
jacobian Contact'{..} (a, b) = ja `join33` jb
  where ja = (-n) `append2` ((xa - p') `cross22` n)
        jb = n `append2` ((p' - xb) `cross22` n)
        xa = _physObjPos a
        xb = _physObjPos b
        p' = view _Point . _neighborhoodCenter $ _contactPenetrator'
        n = _neighborhoodUnitNormal _contactEdge'

-- add extra energy if the penetration exceeds the allowed slop
-- (i.e. subtract from C' = Jv + b in constraint C' <= 0)
baumgarte :: (Fractional a, Ord a)
          => ContactBehavior a
          -> a
          -> Contact' a
          -> a
baumgarte beh dt c = if d > slop then (b / dt) * (slop - d) else 0
  where b = contactBaumgarte beh
        slop = contactPenetrationSlop beh
        d = _contactDepth' c

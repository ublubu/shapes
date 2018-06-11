{-# LANGUAGE RecordWildCards #-}

{- |
Generate and solve non-penetration constraints for colliding objects.
-}
module Physics.Constraints.Contact.NonPenetration where

import Physics.Constraint
import Physics.Constraints.Types
import Physics.Constraints.SolutionProcessors
import Physics.Contact.Types
import Physics.Linear

import Utils.Utils

constraintGen :: ContactBehavior
              -> Double
              -> Flipping Contact'
              -> (PhysicalObj, PhysicalObj)
              -> Constraint
constraintGen beh dt fContact ab =
  flipExtract $ flipMap (toConstraint beh dt) fContact ab

toConstraint :: ContactBehavior
             -> Double
             -> Contact'
             -> (PhysicalObj, PhysicalObj)
             -> Constraint
toConstraint beh dt c ab = Constraint (jacobian c ab) (baumgarte beh dt c)

-- TODO: comment or name stuff so it's clear that `a` is penetrated by `b`
jacobian :: Contact'
         -> (PhysicalObj, PhysicalObj)
         -> V6
jacobian Contact'{..} (a, b) = ja `join3v3` jb
  where ja = negateV2 n `append2` ((xa `minusV2` p') `crossV2` n)
        jb = n `append2` ((p' `minusV2` xb) `crossV2` n)
        xa = _physObjPos a
        xb = _physObjPos b
        (P2 p') = _contactPenetrator'
        n = _contactEdgeNormal'

-- add extra energy if the penetration exceeds the allowed slop
-- (i.e. subtract from C' = Jv + b in constraint C' <= 0)
baumgarte :: ContactBehavior
          -> Double
          -> Contact'
          -> Double
baumgarte beh dt c = if d > slop then (b / dt) * (slop - d) else 0
  where b = contactBaumgarte beh
        slop = contactPenetrationSlop beh
        d = _contactDepth' c

solutionProcessor :: Lagrangian
                  -> Lagrangian
                  -> Processed Lagrangian
solutionProcessor = positive

{-# LANGUAGE RecordWildCards #-}

module Physics.Constraints.Opt.NonPenetration where

import Physics.Constraint.Opt
import Physics.Linear.Opt
import Physics.Contact.Opt

toConstraint :: ContactBehavior
             -> Double
             -> Contact'
             -> (PhysicalObj, PhysicalObj)
             -> Constraint
toConstraint beh dt c ab = Constraint (jacobian c ab) (baumgarte beh dt c)
{-# INLINE toConstraint #-}

-- TODO: comment or name stuff so it's clear that `a` is penetrated by `b`
jacobian :: Contact'
         -> (PhysicalObj, PhysicalObj)
         -> V6
jacobian Contact'{..} (a, b) = ja `join3v3` jb
  where ja = (negateV2 n) `append2` ((xa `minusV2` p') `crossV2` n)
        jb = n `append2` ((p' `minusV2` xb) `crossV2` n)
        xa = _physObjPos a
        xb = _physObjPos b
        (P2 p') = _contactPenetrator'
        n = _contactEdgeNormal'
{-# INLINE jacobian #-}

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
{-# INLINE baumgarte #-}

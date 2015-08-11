module Physics.NonPenetration where

import Control.Lens
import Linear.Affine
import Physics.Constraint
import Physics.Contact
import Physics.Linear

toConstraint_ :: (Fractional a, Ord a) => ContactBehavior a -> a -> Contact a -> Constraint a
toConstraint_ beh dt c = Constraint (jacobian c) (baumgarte beh dt c)

jacobian :: (Num a) => Contact a -> V6 a
jacobian (Contact a b p n _ _) = ja `join33` jb
  where ja = (-n) `append2` ((xa - p') `cross22` n)
        jb = n `append2` ((p' - xb) `cross22` n)
        xa = _physObjPos a
        xb = _physObjPos b
        p' = view _Point p

-- add extra energy if the penetration exceeds the allowed slop
-- (i.e. subtract from C' = Jv + b in constraint C' <= 0)
baumgarte :: (Fractional a, Ord a) => ContactBehavior a -> a -> Contact a -> a
baumgarte beh dt c = if (d > slop) then (b / dt) * (slop - d) else 0
  where b = contactBaumgarte beh
        slop = contactPenetrationSlop beh
        d = contactDepth c

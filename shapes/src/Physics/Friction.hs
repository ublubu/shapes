module Physics.Friction where

import Control.Lens
import Linear.Affine
import Physics.Constraint
import Physics.Contact
import Physics.ContactSolver
import Physics.Linear
import Utils.Utils

toConstraint :: (Fractional a, Ord a) => ContactBehavior a -> a -> Contact a -> Constraint a
toConstraint _ _ c = Constraint (jacobian c) 0

jacobian :: (Num a) => Contact a -> V6 a
jacobian (Contact a b p n _ _) = ja `join33` jb
  where ja = ta `append2` ((p' - xa) `cross22` ta)
        jb = tb `append2` ((p' - xb) `cross22` tb)
        xa = _physObjPos a
        xb = _physObjPos b
        p' = view _Point p
        ta = -tb
        tb = clockwise2 n

pairMu :: (Contactable a p, Fractional a) => (p, p) -> a
pairMu ab = (ua + ub) / 2
  where (ua, ub) = pairMap contactMu ab


{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Generate and solve the "restitution" component of the non-penetration constraint.
-}
module Physics.Constraints.Contact.Restitution where

import           GHC.Prim                               ((*##))
import           GHC.Types                              (Double (D#))

import           Physics.Constraint
import           Physics.Constraints.SolutionProcessors
import           Physics.Constraints.Types
import           Physics.Contact.Types
import           Physics.Linear
import           Physics.World

import           Utils.Utils

constraintGen ::
     Flipping Contact -- ^ contact point
  -> (PhysicalObj, PhysicalObj) -- ^ position
  -> RestitutionConstraint -- ^ radii at the contact point
constraintGen fContact (a, b) =
  RestitutionConstraint {_rcRadiusA = ra, _rcRadiusB = rb, _rcNormal = n}
  where
    P2 center = _contactCenter $ flipExtractUnsafe fContact
    ra = center `minusV2` _physObjPos a
    rb = center `minusV2` _physObjPos b
    n = flipExtractWith (_contactNormal, negateV2 . _contactNormal) fContact

-- | Use the objects' current velocities to calculate the bounce term.
bounceB ::
     (Double, Double) -- ^ bounciness
  -> RestitutionConstraint -- ^ radii at the contact point
  -> (PhysicalObj, PhysicalObj) -- ^ velocity
  -> Double
bounceB bounciness_ab RestitutionConstraint {..} (a, b) =
  min 0 $ D# (bounciness *## (closingVelocity `dotV2` _rcNormal))
  where
    closingVelocity = nva `plusV2` nwa `plusV2` vb `plusV2` wb
    nva = negateV2 $ _physObjVel a
    nwa = (negate $ _physObjRotVel a) `zcrossV2` _rcRadiusA
    vb = _physObjVel b
    wb = _physObjRotVel b `zcrossV2` _rcRadiusB
    D# bounciness = uncurry min bounciness_ab

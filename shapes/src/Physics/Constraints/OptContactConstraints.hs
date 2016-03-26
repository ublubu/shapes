module Physics.Constraints.OptContactConstraints where

import Control.Lens
import Physics.Constraint.OptConstraint
import Physics.Constraint.OptLinear
import Physics.Solver.OptContactSolver
import Physics.Contact.OptContact
import qualified Physics.Constraints.OptFriction as F
import qualified Physics.Constraints.OptNonPenetration as NP
import Utils.Utils

generator :: (Contactable a) => ConstraintGen' a
generator = getGenerator defaultContactBehavior

getGenerator :: (Contactable a)
             => ContactBehavior
             -> ConstraintGen' a
getGenerator beh = ConstraintGen' gen
  where gen dt pairKey ab = (fmap f (generateContacts ab), getGenerator beh)
          where f c = (flipExtractPair contactIndex c, ContactResult (np c) (fr c))
                np c _ = flipConstraint $ flipMap (NP.toConstraint beh dt) c ab'
                fr c _ = flipConstraint $ flipMap (F.toConstraint beh dt) c ab'
                ab' :: (PhysicalObj, PhysicalObj)
                ab' = pairMap (view physObj) ab

flipConstraint :: Flipping Constraint -> Constraint
flipConstraint = flipExtractWith (id, f)
  where f (Constraint j b) = Constraint (flip3v3 j) b

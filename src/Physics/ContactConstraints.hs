module Physics.ContactConstraints where

import Linear.Epsilon
import Physics.Constraint
import Physics.ContactSolver
import Physics.Contact
import Physics.Linear
import qualified Physics.NonPenetration as NP
import qualified Physics.Friction as F
import Utils.Utils

generator :: (Contactable n a, Epsilon n, Floating n, Ord n) => ConstraintGen' n a
generator = getGenerator defaultContactBehavior

getGenerator :: (Physical n a, Epsilon n, Floating n, Ord n) => ContactBehavior n -> ConstraintGen' n a
getGenerator beh dt ab = fmap f (generateContacts (toCP ab))
  where f c = (flipExtractPair contactIndex c, ContactResult (np c) (fr c))
        np c _ = flipConstraint $ fmap (NP.toConstraint beh dt) c
        fr c _ = flipConstraint $ fmap (F.toConstraint beh dt) c

flipConstraint :: Flipping (Constraint a) -> Constraint a
flipConstraint = flipExtractWith (id, f)
  where f (Constraint j b) = Constraint (flip33 j) b

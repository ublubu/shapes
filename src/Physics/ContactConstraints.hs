module Physics.ContactConstraints where

import Linear.Epsilon
import Physics.Constraint
import Physics.ConstraintSolver
import Physics.Contact
import Physics.Linear
import qualified Physics.NonPenetration as NP
import Utils.Utils

generator :: (Physical a n, Epsilon n, Floating n, Ord n) => ConstraintGen n a
generator = getGenerator defaultContactBehavior

getGenerator :: (Physical a n, Epsilon n, Floating n, Ord n) => ContactBehavior n -> ConstraintGen n a
getGenerator beh dt ab = fmap f (generateContacts (toCP ab))
               where f c = (flipExtractPair contactIndex c, const $ toConstraint beh dt c)

toConstraint :: (Fractional a, Ord a) => ContactBehavior a -> a -> Flipping (Contact a) -> Constraint a
toConstraint beh dt c = flipExtractWith (id, f) (fmap (NP.toConstraint_ beh dt) c)
  where f (Constraint j b) = Constraint (flip33 j) b


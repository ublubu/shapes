module Physics.ContactSolver where

import Control.Lens
import Linear.Epsilon
import Physics.Contact
import Physics.Constraint
import Physics.Solver
import Utils.Utils

getSolver :: (Physical a n, Epsilon n, Floating n, Ord n) => ContactBehavior n -> PairSolver n Int a
getSolver beh = Solver (getSolver' beh)

getSolver' :: (Physical a n, Epsilon n, Floating n, Ord n) => ContactBehavior n -> SolverFunc n (Int, Int) (a, a)
getSolver' beh dt ij ab = (foldl f ab cs', getSolver beh)
  where cs' = getGenerator beh dt (g ab)
        f ab' c' = solveConstraint' (c' (g ab')) ab'
        g = pairMap (view physObj)

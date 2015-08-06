module Physics.Solvers where

import Linear.Epsilon
import Physics.Contact as C
import Physics.Constraint
import Physics.ConstraintSolver
import Physics.Solver

contactSolver :: (Physical a n, Epsilon n, Floating n, Ord n) => C.ContactBehavior n -> PairSolver n Int a
contactSolver beh = constraintSolver (C.getGenerator beh)

module Physics.Solvers where

import Linear.Epsilon
import qualified Physics.Contact as C
import qualified Physics.ContactConstraints as CC
import Physics.Constraint
import qualified Physics.ConstraintSolver as CS
import Physics.PairMap
import qualified Physics.SolutionProcessors as SP
import Physics.World
import Physics.WorldSolver

contactSolver :: (Physical a n, Epsilon n, Floating n, Ord n) => C.ContactBehavior n -> WSolver (World a) Key (a, a) n (CS.State a n)
contactSolver beh = (g, f)
  where gen = CC.getGenerator beh
        g = CS.init 0 gen
        f = CS.improve SP.positive

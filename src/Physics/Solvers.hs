module Physics.Solvers where

import Linear.Epsilon
import qualified Physics.Contact as C
import qualified Physics.ContactConstraints as CC
import qualified Physics.ConstraintSolver as CS
import qualified Physics.ContactSolver as ContS
import Physics.PairMap
import qualified Physics.SolutionProcessors as SP
import Physics.World
import Physics.WorldSolver

contactSolver :: (ContS.Contactable n a, Epsilon n, Floating n, Ord n) => C.ContactBehavior n -> WSolver (World a) Key (a, a) n (CS.State n (ContS.Cache n a))
contactSolver beh = (g, f)
  where gen = ContS.generator (CC.getGenerator beh)
        g = CS.init gen
        app = ContS.applicator SP.contact
        f = CS.improve app

module Physics.Solvers.Opt where

import Control.Lens ((%~))
import qualified Data.IntMap.Strict as IM
import qualified Physics.Contact.Opt as C
import qualified Physics.Constraints.Opt.Contact as CC
import qualified Physics.Solver.Constraint as CS
import qualified Physics.Solver.Opt.Contact as ContS
import qualified Physics.Solvers.Opt.SolutionProcessors as SP
import Physics.World.Opt
import Physics.Solver.World
import Utils.PairMap

type ContactSolverState a = CS.ConstraintSolverState (ContS.FeaturePairCaches a) (ContS.WorldCache a)

toShowableSolverState :: ContactSolverState a
                      -> CS.ConstraintSolverState (PairMap (ContS.ContactResult Double)) (ContS.WorldCache a)
toShowableSolverState =
  CS.csPairCaches %~ (fmap . fmap . fmap . fmap $ fst)

contactSolver :: (C.Contactable a)
              => WSolver (World a) Key (a, a) Double (ContactSolverState a)
contactSolver = (g, f)
  where g = CS.initConstraintSolverState ContS.pairCacheInitializer ContS.worldCacheInitializer
        app = ContS.pairUpdater SP.contact
        f = CS.improve app

contactSolver' :: (C.Contactable a)
               => WSolver' (World a) Key (a, a) Double (ContactSolverState a)
contactSolver' = (g, f0, f)
  where (g, f) = contactSolver
        f0 = CS.improve ContS.cacheApplicator

emptyContactSolverState :: (C.Contactable a)
                        => C.ContactBehavior
                        -> ContactSolverState a
emptyContactSolverState beh =
  CS.ConstraintSolverState IM.empty (ContS.WorldCache 0 (CC.getGenerator beh))

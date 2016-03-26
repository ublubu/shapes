module Physics.Solvers.OptSolvers where

import Control.Lens ((%~))
import qualified Data.IntMap.Strict as IM
import qualified Physics.Contact.OptContact as C
import qualified Physics.Constraints.OptContactConstraints as CC
import qualified Physics.ConstraintSolver as CS
import qualified Physics.Solver.OptContactSolver as ContS
import Physics.PairMap
import qualified Physics.Solvers.OptSolutionProcessors as SP
import Physics.World.OptWorld
import Physics.WorldSolver

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

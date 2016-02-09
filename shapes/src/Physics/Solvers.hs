module Physics.Solvers where

import Control.Lens ((&), (%~))
import qualified Data.IntMap.Strict as IM
import Linear.Epsilon
import qualified Physics.Contact as C
import qualified Physics.ContactConstraints as CC
import qualified Physics.ConstraintSolver as CS
import qualified Physics.ContactSolver as ContS
import Physics.PairMap
import qualified Physics.SolutionProcessors as SP
import Physics.World
import Physics.WorldSolver

type ContactSolverState n a = CS.ConstraintSolverState (ContS.FeaturePairCaches n a) (ContS.WorldCache n a)

toShowableSolverState :: ContactSolverState n a -> CS.ConstraintSolverState (PairMap (ContS.ContactResult n)) (ContS.WorldCache n a)
toShowableSolverState =
  CS.csPairCaches %~ (fmap . fmap . fmap . fmap $ fst)

contactSolver :: (C.Contactable n a, Epsilon n, Floating n, Ord n)
              => WSolver (World a) Key (a, a) n (ContactSolverState n a)
contactSolver = (g, f)
  where g = CS.initConstraintSolverState ContS.pairCacheInitializer ContS.worldCacheInitializer
        app = ContS.pairUpdater SP.contact
        f = CS.improve app

contactSolver' :: (C.Contactable n a, Epsilon n, Floating n, Ord n)
               => WSolver' (World a) Key (a, a) n (ContactSolverState n a)
contactSolver' = (g, f0, f)
  where (g, f) = contactSolver
        f0 = CS.improve ContS.cacheApplicator

emptyContactSolverState :: (C.Contactable n a, Epsilon n, Floating n, Ord n)
                        => C.ContactBehavior n
                        -> ContactSolverState n a
emptyContactSolverState beh =
  CS.ConstraintSolverState IM.empty (ContS.WorldCache 0 (CC.getGenerator beh))

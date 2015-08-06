module Physics.ConstraintSolver where

import Control.Lens
import qualified Data.IntMap.Strict as IM
import Data.Maybe
import Linear.Epsilon
import Physics.Constraint
import Physics.Solver
import Utils.Utils

type ConstraintGen n = n -> ConstrainedPair n -> [Constraint' n]
type PairMap a = IM.IntMap (IM.IntMap a)

findOrInsertPair :: (Int, Int) -> a -> PairMap a -> (Maybe a, PairMap a)
findOrInsertPair (i, j) x t = (mx', IM.insert i tt' t)
  where mtt = IM.lookup i t
        (mx', tt') = case mtt of Just tt -> findOrInsert j x tt
                                 Nothing -> (Nothing, IM.insert j x IM.empty)
        t' = case mx' of Just _ -> t -- x was not inserted
                         Nothing -> IM.insert i tt' t -- x was inserted, insert updated tt'

findOrInsertPair' :: (Int, Int) -> a -> PairMap a -> (a, PairMap a)
findOrInsertPair' ij x t = (fromMaybe x mx, t')
  where (mx, t') = findOrInsertPair ij x t

constraintSolver :: (Physical a n, Epsilon n, Floating n, Ord n) => ConstraintGen n -> PairSolver n Int a
constraintSolver gen = makeSolver f g (0, IM.empty)
  where f k a s@(dt, cache) = (a', s')
          where s' = s & _2 .~ cache'
                solveOne a0 c' = solveConstraint' (cpMap c' a0) a0
                a' = foldl solveOne a cs'
                (cs', cache') = findOrInsertPair' k (cpMap (gen dt) a) cache
        g dt s = (dt, IM.empty)

toCP :: (Physical a n) => (a, a) -> ConstrainedPair n
toCP = pairMap (view physObj)

cpMap :: (Physical a n) => (ConstrainedPair n -> b) -> (a, a) -> b
cpMap f pair = f (toCP pair)

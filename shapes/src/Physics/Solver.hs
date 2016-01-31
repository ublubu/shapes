{-# LANGUAGE FlexibleInstances, FlexibleContexts, RankNTypes #-}

module Physics.Solver where

import Control.Applicative
import Control.Lens

data Solver x k a = Solver !(SolverFunc x k a) !(SolverGen x k a)
type SolverGen x k a = x -> Solver x k a
type SolverFunc x k a = k -> a -> (a, Solver x k a)
type PairSolver x k a = Solver x (k, k) (a, a)

solve :: IndexedTraversal' k s a -> s -> Solver x k a -> (s, Solver x k a)
solve l s sf@(Solver f _) = case ms of Just (a, f') -> (s & l .~ a, f')
                                       Nothing -> (s, sf)
  where ms = fmap (uncurry f) (s ^@? l)

solverGen :: Solver x k a -> SolverGen x k a
solverGen (Solver _ g) = g

solverFunc :: Solver x k a -> SolverFunc x k a
solverFunc (Solver f _) = f

makeSolver :: (k -> a -> s -> (a, s)) -> (x -> s -> s) -> s -> Solver x k a
makeSolver f g s = Solver f' g'
  where f' k a = (a', makeSolver f g s')
          where (a', s') = f k a s
        g' x = makeSolver f g (g x s)

{-# LANGUAGE FlexibleInstances, FlexibleContexts, RankNTypes #-}

module Physics.Solver where

import Control.Applicative
import Control.Lens

data Solver x k a = Solver (SolverFunc x k a) (SolverGen x k a)
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

pairiix :: (Ixed m) => (Index m, Index m) -> IndexedTraversal' (Index m, Index m) m (IxValue m, IxValue m)
pairiix ij f = pairix ij (indexed f ij)

-- Applicative f => (IxValue m -> f (IxValue m)) -> m -> f m
pairix :: (Ixed m) => (Index m, Index m) -> Traversal' m (IxValue m, IxValue m)
pairix ij@(i, j) f t = maybe (pure t) change pair
  where pair = do
          a <- t ^? ix i
          b <- t ^? ix j
          return (a, b)
        change pair' = uncurry g <$> indexed f ij pair'
          where g a b = set (ix j) b . set (ix i) a $ t

makeSolver :: (k -> a -> s -> (a, s)) -> (x -> s -> s) -> s -> Solver x k a
makeSolver f g s = Solver f' g'
  where f' k a = (a', makeSolver f g s')
          where (a', s') = f k a s
        g' x = makeSolver f g (g x s)

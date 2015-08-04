{-# LANGUAGE FlexibleInstances, FlexibleContexts, RankNTypes #-}

module Physics.Solver where

import Control.Applicative
import Control.Lens

data Solver x k a = Solver (SolverFunc x k a)
type SolverFunc x k a = x -> k -> a -> (a, Solver x k a)
type PairSolver x k a = Solver x (k, k) (a, a)

solve' :: IndexedTraversal' k s a -> Solver x k a -> x -> s -> (s, Solver x k a)
solve' l sf@(Solver f) x s = case ms of Just (a, f') -> (s & l .~ a, f')
                                        Nothing -> (s, sf)
  where ms = fmap (uncurry (f x)) (s ^@? l)

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

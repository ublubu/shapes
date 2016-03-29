{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Physics.Solver.World where

import Control.Lens

type SolverGen x s = x -> s -> s
type SolverFunc k a s = k -> a -> s -> (a, s)

solve :: SolverFunc k a s -> IndexedTraversal' k w a -> w -> s -> (w, s)
solve f l w s = case ma of Just (k, a) -> (w', s')
                                where (a', s') = f k a s
                                      w' = w & l .~ a'
                           Nothing -> (w, s)
  where ma = w ^@? l

{-
k = key (object index)
w = world (contains objects)
a = object type
x = timestep
s = solver state (cache)
-}
type WorldLens k w a = k -> Traversal' w a
-- WSGen :: generate new solver state (update cache)
type WSGen w k a x s = [k] -> WorldLens k w a -> w -> x -> s -> s
-- WSFunc :: update world & solver state
type WSFunc w k a s = WorldLens k w a -> w -> s -> (w, s)
-- WSChanged :: did the world actually change? (threshold for early exit)
type WSChanged w = w -> w -> Bool
-- WSolver :: everything you need to step the world
type WSolver w k a x s = (WSGen w k a x s, WSFunc w k a s)
-- WSolver' :: use one WSFunc to seed the first solver iteration, use the other for subsequent iterations (seed & improve)
type WSolver' w k a x s = (WSGen w k a x s, WSFunc w k a s, WSFunc w k a s)

-- run gen to reset solver state
-- iteratively run func to improve solution (until unchanged; up to n times)
wsolve_ :: WSGen w k a x s -> WSFunc w k a s -> WSChanged w -> Int -> [k] -> WorldLens k w a -> w -> x -> s -> (w, s)
wsolve_ g f changed n ks l w x s = wsimprove f changed n l w s1
  where s1 = g ks l w x s

wsolve :: WSolver w k a x s -> WSChanged w -> Int -> [k] -> WorldLens k w a -> w -> x -> s -> (w, s)
wsolve = uncurry wsolve_

-- use WSGen to fill the solver cache before solving with wsimprove'
wsolve_' :: WSGen w k a x s -> WSFunc w k a s -> WSFunc w k a s -> WSChanged w -> Int -> [k] -> WorldLens k w a -> w -> x -> s -> (w, s)
wsolve_' g f0 f changed n ks l w x s = wsimprove' f0 f changed n l w s1
  where s1 = g ks l w x s

wsolve' :: WSolver' w k a x s -> WSChanged w -> Int -> [k] -> WorldLens k w a -> w -> x -> s -> (w, s)
wsolve' (g, f0, f) = wsolve_' g f0 f

-- run func to improve solution until unchanged (max n iterations)
wsimprove :: WSFunc w k a s -> WSChanged w -> Int -> WorldLens k w a -> w -> s -> (w, s)
wsimprove _ _       0 _ w s = (w, s)
wsimprove f changed n l w s = (w', s')
  where (w1, s1) = f l w s
        (w', s') = if changed w w1 then wsimprove f changed (n - 1) l w1 s1
                   else (w1, s1)

-- apply first solver function (WSFunc) to seed the solution
-- iteratively apply second solver function to improve the solution
wsimprove' :: WSFunc w k a s -> WSFunc w k a s -> WSChanged w -> Int -> WorldLens k w a -> w -> s -> (w, s)
wsimprove' f0 f1 changed n l w s = wsimprove f1 changed n l w' s'
  where (w', s') = f0 l w s

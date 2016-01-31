{-# LANGUAGE FlexibleInstances, FlexibleContexts, RankNTypes #-}

module Physics.ConstraintSolver where

import Control.Lens
import qualified Data.IntMap.Strict as IM
import Data.Maybe
import Physics.Constraint
import Physics.PairMap
import Physics.World hiding (solveOne, solveMany)
import Physics.WorldSolver
import Utils.Utils

type Generator n a c = n -> (a, a) -> Maybe c -> c
type Applicator a c = (a, a) -> c -> ((a, a), c)
type State n c = (n, PairMap c)
type WorldLens2 k w a = WorldLens k w (a, a)

emptyState :: (Num n) => State n c
emptyState = (0, IM.empty)

-- s0 is the previous solver state
-- result is next state
init :: Generator n a c -> WSGen (World a) Key (a, a) n (State n c)
init g ks l w x s0 = foldl f (x, IM.empty) ks
  where f s k = initOne g k l w x s0 s

-- initialize one PairMap entry
-- TODO: have Generator return (Maybe c) to avoid empty state entries
initOne :: Generator n a c -> Key -> WorldLens2 Key (World a) a -> World a -> n -> State n c -> State n c -> State n c
initOne g k l w x s_src s_accum = s_accum & _2 %~ insertPair k cache
  where ab = fromJust $ w ^? l k
        caches0 = s_src ^. _2
        cache = g x ab (lookupPair k caches0)

improve :: Applicator a c -> WSFunc (World a) Key (a, a) (State n c)
improve f l w s = (w', s')
  where (w', s') = foldl f' (w, s) ks
        f' (w0, s0) k = improveOne f k l w0 s0
        ks = keys (snd s)

improveOne :: Applicator a c -> Key -> WorldLens2 Key (World a) a -> World a -> State n c -> (World a, State n c)
improveOne f k l w s = (w & l k .~ ab', s & _2 %~ insertPair k cache')
  where cache = fromJust $ lookupPair k (snd s)
        ab = fromJust $ w ^? l k
        (ab', cache') = f ab cache

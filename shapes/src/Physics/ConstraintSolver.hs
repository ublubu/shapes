{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}

module Physics.ConstraintSolver where

import Control.Lens
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import Data.Maybe
import Physics.Constraint
import Physics.PairMap
import Physics.World hiding (solveOne, solveMany)
import Physics.WorldSolver
import Utils.Utils

{-
n/x = dt (number type)
a = object
c = pairwise cache
wc = world cache
w = world
-}

-- initialize solver cache for a pair of objects (may affect global cache)
-- (was: Generator)
type PairCacheInitializer a c wc = Key -> (a, a) -> Maybe c -> wc -> (Maybe c, wc)

-- use caches to update objects (caches are updated as well)
-- (was: Applicator)
type PairUpdater a c wc = Key -> (a, a) -> c -> wc -> ((a, a), c, wc)

-- initialize global solver cache
type WorldCacheInitializer a x wc = WSGen (World a) Key (a, a) x wc

-- (was: State)
data ConstraintSolverState c wc =
  ConstraintSolverState { _csPairCaches :: !(PairMap c)
                        , _csWorldCache :: !wc
                        }
makeLenses ''ConstraintSolverState

initConstraintSolverState :: PairCacheInitializer a c wc
                          -> WorldCacheInitializer a x wc
                          -> WSGen (World a) Key (a, a) x (ConstraintSolverState c wc)
initConstraintSolverState pairCacheInit worldCacheInit pairKeys l world dt csState0 =
  foldl' f (ConstraintSolverState IM.empty worldCache1) pairKeys
  where worldCache1 = worldCacheInit pairKeys l world dt (csState0 ^. csWorldCache)
        pairCaches0 = csState0 ^. csPairCaches
        f !csState !pairKey = initOnePairCache pairCacheInit pairKey l world pairCaches0 csState

initOnePairCache :: PairCacheInitializer a c wc
                 -> Key
                 -> WorldLens Key (World a) (a, a)
                 -> World a
                 -> PairMap c
                 -> ConstraintSolverState c wc
                 -> ConstraintSolverState c wc
initOnePairCache pairCacheInit pairKey l world pairCachesSrc csStateAccum =
  csStateAccum & csPairCaches %~ setPairCache & csWorldCache .~ worldCache
  where ab = fromJust $ world ^? l pairKey
        (pairCache, worldCache) = pairCacheInit pairKey ab
                                   (lookupPair pairKey pairCachesSrc)
                                   (csStateAccum ^. csWorldCache)
        setPairCache = maybe id (insertPair pairKey) pairCache

improve :: PairUpdater a c wc -> WSFunc (World a) Key (a, a) (ConstraintSolverState c wc)
improve pairUpdater l world0 csState0 =
  foldl' f (world0, csState0) pairKeys
  where f (world, csState) pairKey =
          improveOne pairUpdater pairKey l world csState
        pairKeys = keys (csState0 ^. csPairCaches)

improveOne :: PairUpdater a c wc
           -> Key
           -> WorldLens Key (World a) (a, a)
           -> World a
           -> ConstraintSolverState c wc
           -> (World a, ConstraintSolverState c wc)
improveOne pairUpdater pairKey l world csState =
  (world', csState')
  where ab = fromJust $ world ^? l pairKey
        worldCache = csState ^. csWorldCache
        pairCache = fromJust $ lookupPair pairKey (csState ^. csPairCaches)
        (ab', pairCache', worldCache') = pairUpdater pairKey ab pairCache worldCache
        world' = world & l pairKey .~ ab'
        csState' = csState & csPairCaches %~ insertPair pairKey pairCache'
                   & csWorldCache .~ worldCache'

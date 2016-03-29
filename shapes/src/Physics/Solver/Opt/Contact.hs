{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}

module Physics.Solver.Opt.Contact where

import Control.Lens
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import Data.Maybe
import Physics.Constraint.Opt
import Physics.Contact.Opt
import Physics.World.Opt
import qualified Physics.Solver.Constraint as CS
import Utils.PairMap

data ContactResult x = ContactResult { _contactNonPen :: !x
                                     , _contactFriction :: !x
                                     } deriving (Show, Eq)
makeLenses ''ContactResult

-- use to generate constraints (jacobians)
type ConstraintGen a = ContactResult (Constraint' a)

-- cache constraint impulses
--type SolutionCache n = ContactResult n

-- ConstraintGen' = dt -> (obj1, obj2) -> ([((featIndex1, featIndex2), ConstraintGen)], new ConstraintGen')
newtype ConstraintGen' a =
  ConstraintGen' { _cgen' :: Double
                          -> Key
                          -> (a, a)
                          -> ([(Key, ConstraintGen a)], ConstraintGen' a)
                 }
makeLenses ''ConstraintGen'

-- per-contact (feature pair) cache:
type FeaturePairCaches a = PairMap (ContactResult Double, ConstraintGen a)

data WorldCache a =
  WorldCache { _wcDt :: Double
             , _wcCgen' :: ConstraintGen' a
             }
makeLenses ''WorldCache

instance Show (WorldCache a) where
  show WorldCache{..} =
    "WorldCache " ++ show _wcDt

wcApplyCgen' :: WorldCache a
             -> Key
             -> (a, a)
             -> ([(Key, ConstraintGen a)], WorldCache a)
wcApplyCgen' worldCache@WorldCache{..} pairKey ab =
  (featurePairCgens, worldCache & wcCgen' .~ newCgen')
  where (featurePairCgens, newCgen') = _cgen' _wcCgen' _wcDt pairKey ab

-- apply rules to total constraint impulse
-- (previously accumulated impulse) -> (current iteration's sln) -> (objects) -> (new accumulated impulse, incremental sln to apply)
type SolutionProcessor a = ContactResult Double -> ContactResult ConstraintResult -> (a, a) -> (ContactResult Double, ContactResult ConstraintResult)

instance Functor ContactResult where
  fmap f (ContactResult a b) = ContactResult (f a) (f b)

instance Applicative ContactResult where
  pure f = ContactResult f f
  (ContactResult f g) <*> (ContactResult x y) = ContactResult (f x) (g y)

emptySln :: (Num n) => ContactResult n
emptySln = ContactResult 0 0

-- copy the last frame's cached solution if it exists
-- cache the ConstraintGens for this frame
updatePairCache :: [(Key, ConstraintGen a)]
                   -> ContactResult Double
                   -> Maybe (FeaturePairCaches a)
                   -> Maybe (FeaturePairCaches a)
updatePairCache [] _ _ = Nothing -- no contacts this frame = clear everything
updatePairCache featurePairCgens emptySolution (Just featurePairCaches0) =
  Just $ foldl' f IM.empty featurePairCgens
  where f !featurePairCaches (!pairKey, !cgen) =
          insertPair pairKey (sln', cgen) featurePairCaches
          where sln' = maybe emptySolution fst $ lookupPair pairKey featurePairCaches0
updatePairCache featurePairCgens emptySolution Nothing =
  Just $ foldl' f IM.empty featurePairCgens
  where f !featurePairCaches (!pairKey, !cgen) =
          insertPair pairKey (emptySolution, cgen) featurePairCaches

pairCacheInitializer :: CS.PairCacheInitializer a (FeaturePairCaches a) (WorldCache a)
pairCacheInitializer pairKey ab mFeaturePairCaches worldCache =
  (mFeaturePairCaches', worldCache')
  where mFeaturePairCaches' = updatePairCache featurePairCgens emptySln mFeaturePairCaches
        (featurePairCgens, worldCache') = wcApplyCgen' worldCache pairKey ab

worldCacheInitializer :: CS.WorldCacheInitializer World a Double (WorldCache a)
worldCacheInitializer pairKeys l world dt worldCache = worldCache & wcDt .~ dt

-- TODO: traverse/fold these IntMaps directly instead of folding over keys
--       to get rid of some of these fromJusts (also in ConstraintSolver)
pairUpdater :: (Contactable a)
            => SolutionProcessor a
            -> CS.PairUpdater a (FeaturePairCaches a) (WorldCache a)
pairUpdater slnProc pairKey ab0 featurePairCaches0 worldCache0 =
  (ab', featurePairCaches', worldCache0)
  where (ab', featurePairCaches') =
          solveMany slnProc (keys featurePairCaches0) featurePairCaches0 ab0

-- apply the cached solutions
cacheApplicator :: (Contactable a)
                => CS.PairUpdater a (FeaturePairCaches a) (WorldCache a)
cacheApplicator pairKey ab0 featurePairCaches0 worldCache0 =
  (foldl' f ab0 (keys featurePairCaches0), featurePairCaches0, worldCache0)
  where f !ab !featurePairKey = applyContactConstraintResult constraintResult' ab
          where (sln, cgen) = fromJust $ lookupPair featurePairKey featurePairCaches0
                constraint = fmap ($ ab0) cgen
                constraintResult' = (,) <$> sln <*> constraint

solveMany :: (Contactable a)
          => SolutionProcessor a
          -> [Key]
          -> FeaturePairCaches a
          -> (a, a)
          -> ((a, a), FeaturePairCaches a)
solveMany slnProc featurePairKeys featurePairCaches0 ab0 =
  foldl f (ab0, featurePairCaches0) featurePairKeys
  where f (ab, featurePairCaches) featurePairKey =
          (ab', insertPair featurePairKey (sln', cgen) featurePairCaches)
          where (ab', sln') = solveOne slnProc featurePairKey cgen ab sln
                (sln, cgen) = fromJust $ lookupPair featurePairKey featurePairCaches

solveOne :: (Contactable a)
         => SolutionProcessor a
         -> Key
         -> ConstraintGen a
         -> (a, a)
         -> ContactResult Double
         -> ((a, a), ContactResult Double)
solveOne sp k cg ab sln = (ab', sln')
  where ab' = applyContactConstraintResult cr' ab
        cr = fmap (`constraintResult` ab) cg
        (sln', cr') = sp sln cr ab

applyContactConstraintResult :: (Contactable a)
                             => ContactResult ConstraintResult
                             -> (a, a)
                             -> (a, a)
applyContactConstraintResult (ContactResult cr cr') = applyConstraintResult cr' . applyConstraintResult cr

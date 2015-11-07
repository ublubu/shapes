{-# LANGUAGE FlexibleInstances, FlexibleContexts, RankNTypes, MultiParamTypeClasses, TemplateHaskell #-}

module Physics.ContactSolver where

import Control.Applicative
import Control.Lens
import qualified Data.IntMap.Strict as IM
import Data.Maybe
import Physics.Constraint
import Physics.PairMap
import Physics.World hiding (solveOne, solveMany)
import Physics.WorldSolver
import qualified Physics.ConstraintSolver as CS
import Utils.Utils

class (Physical a p) => Contactable a p where
  contactMu :: p -> a

data ContactResult x = ContactResult { _contactNonPen :: !x
                                     , _contactFriction :: !x } deriving Show
makeLenses ''ContactResult
type ConstraintGen n a = ContactResult (Constraint' n)
type SolutionCache n = ContactResult n
type ConstraintGen' n a = n -> (a, a) -> [(Key, ConstraintGen n a)]
type Cache n a = PairMap (SolutionCache n, ConstraintGen n a)
type SolutionProcessor n a = SolutionCache n -> ContactResult (ConstraintResult n) -> (a, a) -> (SolutionCache n, ContactResult (ConstraintResult n))

instance Functor ContactResult where
  fmap f (ContactResult a b) = ContactResult (f a) (f b)

instance Applicative ContactResult where
  pure f = ContactResult f f
  (ContactResult f g) <*> (ContactResult x y) = ContactResult (f x) (g y)

emptySln :: (Num n) => SolutionCache n
emptySln = ContactResult 0 0

generator :: (Contactable n a, Num n) => ConstraintGen' n a -> CS.Generator n a (Cache n a)
generator cg dt ab mc = initCache emptySln mc (cg dt ab)

-- copy the last frame's SolutionCache if it exists
-- generate and cache new ConstraintGens
initCache :: (Num n) => SolutionCache n -> Maybe (Cache n a) -> [(Key, ConstraintGen n a)] -> Cache n a
initCache cache0 (Just cache) cgs = foldl f IM.empty cgs
  where f cache' (k, cg) = insertPair k (sln', cg) cache'
          where sln' = case lookupPair k cache of
                  Just (sln, _) -> sln
                  Nothing -> cache0
initCache cache0 Nothing cgs = foldl f IM.empty cgs
  where f cache' (k, cg) = insertPair k (cache0, cg) cache'

applicator :: (Contactable n a, Fractional n) => SolutionProcessor n a -> CS.Applicator a (Cache n a)
applicator sp ab cache = solveMany sp (keys cache) cache ab

applicator' :: (Contactable n a, Fractional n) => CS.Applicator a (Cache n a)
applicator' ab cache = (foldl f ab (keys cache), cache)
  where f ab0 k = ab'
          where (sln, cg) = fromJust $ lookupPair k cache
                c = fmap (`constraint` ab0) cg
                cr = (,) <$> sln <*> c
                ab' = applyContactConstraintResult cr ab0

solveMany :: (Contactable n a, Fractional n) => SolutionProcessor n a -> [Key] -> Cache n a -> (a, a) -> ((a, a), Cache n a)
solveMany sp ks cache ab = foldl f (ab, cache) ks
  where f (ab0, cache0) k = (ab', insertPair k (sln', c') cache0)
          where (ab', sln') = solveOne sp k c' ab0 sln0
                (sln0, c') = fromJust $ lookupPair k cache0

solveOne :: (Contactable n a, Fractional n) => SolutionProcessor n a -> Key -> ConstraintGen n a -> (a, a) -> SolutionCache n -> ((a, a), SolutionCache n)
solveOne sp k cg ab sln = (ab', sln')
  where ab' = applyContactConstraintResult cr' ab
        cr = fmap (`constraintResult` ab) cg
        (sln', cr') = sp sln cr ab

applyContactConstraintResult :: (Contactable n a, Fractional n) => ContactResult (ConstraintResult n) -> (a, a) -> (a, a)
applyContactConstraintResult (ContactResult cr cr') = applyConstraintResult cr' . applyConstraintResult cr

{-# LANGUAGE TemplateHaskell #-}

module Physics.World where

import Control.Applicative
import Control.Lens
import Data.Maybe
import Data.Sequence
import Data.Sequence.Lens
import Physics.Constraint hiding (solveConstraint)
import qualified Physics.Constraint as C
import Linear.Epsilon
import Linear.Vector

data World a = World { _worldObjs :: Seq (PhysicalObj a) } deriving Show
makeLenses ''World;

ixWorldPair :: Applicative f => (Int, Int) -> (ConstrainedPair a -> f (ConstrainedPair a)) -> World a -> f (World a)
ixWorldPair (i, j) f w = maybe (pure w) change pair
  where pair = do
          a <- w ^? worldObjs.ix i
          b <- w ^? worldObjs.ix j
          return (a, b)
        change pair' = uncurry g <$> f pair'
          where g a b = set (worldObjs.ix j) b . set (worldObjs.ix i) a $ w

testWorld = World (fromList [testObj, testObj])

data WorldPair a = WorldPair (Int, Int) a deriving Show
type External a = PhysicalObj a -> a -> PhysicalObj a

instance Functor WorldPair where
  fmap f (WorldPair ij x) = WorldPair ij (f x)

fromPair :: WorldPair a -> a
fromPair (WorldPair _ a) = a

advanceWorld :: (Num a) => a -> World a -> World a
advanceWorld dt w = w & worldObjs %~ fmap (`advanceObj` dt)

allPairs :: World a -> [WorldPair (ConstrainedPair a)]
allPairs w = ifoldlOf (worldObjs.traversed) f [] w
  where f i a b = ifoldlOf (worldObjs.slicedTo i) g a w
          where g j ps b' = WorldPair (i, j) (b, b') : ps

getPair :: World a -> (Int, Int) -> ConstrainedPair a
getPair w (i, j) = (f i, f j)
  where f k = fromJust $ w ^? worldObjs.(ix k)

constraints :: (Epsilon a, Floating a, Ord a) => World a -> [ConstraintGen a] -> [WorldPair (Constraint' a)]
constraints w gens = foldl (\cs pair -> foldl (f pair) cs gens) [] (allPairs w)
  where f (WorldPair ij pair) cs gen = fmap (WorldPair ij) (gen pair) ++ cs

solveConstraint :: (Epsilon a, Floating a, Ord a) => World a -> WorldPair (Constraint' a) -> World a
solveConstraint w (WorldPair ij c') = w & ixWorldPair ij %~ C.solveConstraint c
  where c = c' . fromJust $ (w ^? ixWorldPair ij)

-- TODO: measure velocity change from each constraint solve to determine convergence
solveConstraints :: (Epsilon a, Floating a, Ord a) => World a -> [WorldPair (Constraint' a)] -> World a
solveConstraints = foldl solveConstraint

solveGens :: (Epsilon a, Floating a, Ord a) => [ConstraintGen a] -> World a -> World a
solveGens gs w = solveConstraints w (constraints w gs)


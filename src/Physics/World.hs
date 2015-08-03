{-# LANGUAGE TemplateHaskell #-}

module Physics.World where

import Control.Applicative
import Control.Lens
import qualified Data.IntMap.Strict as IM
import Data.Maybe
import Physics.Constraint hiding (solveConstraint)
import qualified Physics.Constraint as C
import Linear.Epsilon
import Linear.Vector
import Utils.Utils

data World a = World { _worldObjs :: IM.IntMap (PhysicalObj a)
                     , _worldNextKey :: Int } deriving Show
makeLenses ''World

emptyWorld :: World a
emptyWorld = World IM.empty 0

addObj :: World a -> PhysicalObj a -> World a
addObj w o = w & worldObjs %~ IM.insert n o & worldNextKey .~ n + 1
  where n = w ^. worldNextKey

ixWorldPair :: Applicative f => (Int, Int) -> (ConstrainedPair a -> f (ConstrainedPair a)) -> World a -> f (World a)
ixWorldPair (i, j) f w = maybe (pure w) change pair
  where pair = do
          a <- w ^? worldObjs.ix i
          b <- w ^? worldObjs.ix j
          return (a, b)
        change pair' = uncurry g <$> f pair'
          where g a b = set (worldObjs.ix j) b . set (worldObjs.ix i) a $ w

fromList :: [PhysicalObj a] -> World a
fromList = foldl addObj emptyWorld

testWorld = fromList [testObj, testObj]

-- TODO: make ConstraintGen and External indexed functions (?) so there can be object-specific behavior.
data WorldPair a = WorldPair (Int, Int) a deriving Show
type External a = a -> PhysicalObj a -> PhysicalObj a
type WorldChanged a = World a -> World a -> Bool
data WorldBehavior a = WorldBehavior [ConstraintGen a] [External a] (WorldChanged a) Int

instance Functor WorldPair where
  fmap f (WorldPair ij x) = WorldPair ij (f x)

fromPair :: WorldPair a -> a
fromPair (WorldPair _ a) = a

advanceWorld :: (Num a) => a -> World a -> World a
advanceWorld dt w = w & worldObjs %~ fmap (`advanceObj` dt)

allPairs :: World a -> [WorldPair (ConstrainedPair a)]
allPairs w = fst $ ifoldlOf (worldObjs.traversed) f ([], []) w
  where f i (pairs, xs) x = (foldl g pairs xs, (i, x):xs)
          where g ps (j, x') = WorldPair (i, j) (x, x') : ps

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

-- generate constraints
-- apply externals
-- solve constraints (sequential impulses until error is gone)
-- update position
updateWorld :: (Epsilon a, Floating a, Ord a, Num b, Ord b) => WorldBehavior a -> a -> World a -> World a
updateWorld (WorldBehavior gens exts changed n) dt w = advanceWorld dt w''
  where cs = constraints w gens
        w' = foldl (\ww ext -> ww & worldObjs.traverse %~ ext dt) w exts
        w'' = f n w'
        f 0 ww = ww
        f n' ww = if changed ww ww' then f (n' - 1) ww'
                  else ww'
          where ww' = solveConstraints ww cs

worldChanged :: PhysObjChanged a -> WorldChanged a
worldChanged objChanged w w' = anyOf traversed id (ixZipWith f os os')
  where f o mo' = case mo' of
          Just o' -> objChanged o o'
          Nothing -> False
        os = w ^. worldObjs
        os' = w' ^. worldObjs


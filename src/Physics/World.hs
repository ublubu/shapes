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

data World a = World { _worldObjs :: IM.IntMap a
                     , _worldNextKey :: Int } deriving Show
makeLenses ''World

emptyWorld :: World a
emptyWorld = World IM.empty 0

addObj :: World a -> a -> World a
addObj w o = w & worldObjs %~ IM.insert n o & worldNextKey .~ n + 1
  where n = w ^. worldNextKey

ixWorldPair :: Applicative f => (Int, Int) -> ((a, a) -> f (a, a)) -> World a -> f (World a)
ixWorldPair (i, j) f w = maybe (pure w) change pair
  where pair = do
          a <- w ^? worldObjs.ix i
          b <- w ^? worldObjs.ix j
          return (a, b)
        change pair' = uncurry g <$> f pair'
          where g a b = set (worldObjs.ix j) b . set (worldObjs.ix i) a $ w

fromList :: [a] -> World a
fromList = foldl addObj emptyWorld

testWorld = fromList [testObj, testObj]

data WorldPair a = WorldPair (Int, Int) a deriving Show
type External' n = n -> PhysicalObj n -> PhysicalObj n
type External n a = n -> a -> a
type WorldChanged a = World a -> World a -> Bool
data WorldBehavior n a = WorldBehavior [ConstraintGen n a] [External n a] (WorldChanged a) Int
type ConstraintGen n a = n -> (a, a) -> [Constraint' n]

instance Functor WorldPair where
  fmap f (WorldPair ij x) = WorldPair ij (f x)

fromPair :: WorldPair a -> a
fromPair (WorldPair _ a) = a

advanceWorld :: (Physical a n, Num n) => n -> World a -> World a
advanceWorld dt w = w & worldObjs.traverse.physObj %~ (`advanceObj` dt)

allPairs :: World a -> [WorldPair (a, a)]
allPairs w = fst $ ifoldlOf (worldObjs.traversed) f ([], []) w
  where f i (pairs, xs) x = (foldl g pairs xs, (i, x):xs)
          where g ps (j, x') = WorldPair (i, j) (x, x') : ps

getPair :: World a -> (Int, Int) -> (a, a)
getPair w (i, j) = (f i, f j)
  where f k = fromJust $ w ^? worldObjs.(ix k)

wrapExternal :: (Physical a n) => External' n -> External n a
wrapExternal f dt = over physObj (f dt)

constraints :: (Physical a n, Epsilon n, Floating n, Ord n) => n -> World a -> [ConstraintGen n a] -> [WorldPair (Constraint' n)]
constraints dt w gens = foldl (\cs pair -> foldl (f pair) cs gens) [] (allPairs w)
  where f (WorldPair ij pair) cs gen = fmap (WorldPair ij) (gen dt pair) ++ cs

solveConstraint :: (Physical a n, Epsilon n, Floating n, Ord n) => World a -> WorldPair (Constraint' n) -> World a
solveConstraint w (WorldPair ij c') = w & ixWorldPair ij %~ f
  where f = C.solveConstraint' c
        c = c' . pairMap (view physObj). fromJust $ (w ^? ixWorldPair ij)

-- TODO: measure velocity change from each constraint solve to determine convergence
solveConstraints :: (Physical a n, Epsilon n, Floating n, Ord n) => World a -> [WorldPair (Constraint' n)] -> World a
solveConstraints = foldl solveConstraint

solveGens :: (Physical a n, Epsilon n, Floating n, Ord n) => [ConstraintGen n a] -> n -> World a -> World a
solveGens gs dt w = solveConstraints w (constraints dt w gs)

-- generate constraints
-- apply externals
-- solve constraints (sequential impulses until error is gone)
-- update position
updateWorld :: (Physical a n, Epsilon n, Floating n, Ord n) => WorldBehavior n a -> n -> World a -> World a
updateWorld (WorldBehavior gens exts changed n) dt w = advanceWorld dt w''
  where cs = constraints dt w gens
        w' = foldl (\ww ext -> ww & worldObjs.traverse %~ ext dt) w exts
        w'' = f n w'
        f 0 ww = ww
        f n' ww = if changed ww ww' then f (n' - 1) ww'
                  else ww'
          where ww' = solveConstraints ww cs

worldChanged :: (Physical a n) => PhysObjChanged n -> WorldChanged a
worldChanged objChanged w w' = anyOf traverse id (ixZipWith f os os')
  where f o mo' = case mo' of
          Just o' -> objChanged (o ^. physObj) (o' ^. physObj)
          Nothing -> False
        os = w ^. worldObjs
        os' = w' ^. worldObjs


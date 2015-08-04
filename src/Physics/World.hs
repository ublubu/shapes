{-# LANGUAGE TemplateHaskell, FlexibleContexts, RankNTypes #-}

module Physics.World where

import Control.Applicative
import Control.Lens
import Control.Lens.Indexed
import qualified Data.IntMap.Strict as IM
import Data.Maybe
import Physics.Constraint hiding (solveConstraint)
import Physics.Solver
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

fromList :: [a] -> World a
fromList = foldl addObj emptyWorld

testWorld = fromList [testObj, testObj]

data WorldPair a = WorldPair (Int, Int) a deriving Show
type External' n = n -> PhysicalObj n -> PhysicalObj n
type External n a = n -> a -> a
type WorldChanged a = World a -> World a -> Bool
data WorldBehavior n a = WorldBehavior { _worldSolvers :: [PairSolver n Int a]
                                       , _worldExternals :: [External n a]
                                       , _worldChanged :: WorldChanged a
                                       , _worldMaxIterations :: Int }
makeLenses ''WorldBehavior

instance Functor WorldPair where
  fmap f (WorldPair ij x) = WorldPair ij (f x)

fromPair :: WorldPair a -> a
fromPair (WorldPair _ a) = a

pairIndex :: WorldPair a -> (Int, Int)
pairIndex (WorldPair ij _) = ij

advanceWorld :: (Physical a n, Num n) => n -> World a -> World a
advanceWorld dt w = w & worldObjs.traverse.physObj %~ (`advanceObj` dt)

allPairs :: World a -> [WorldPair (a, a)]
allPairs w = fst $ ifoldlOf (worldObjs.traversed) f ([], []) w
  where f i (pairs, xs) x = (foldl g pairs xs, (i, x):xs)
          where g ps (j, x') = WorldPair (i, j) (x, x') : ps

wrapExternal :: (Physical a n) => External' n -> External n a
wrapExternal f dt = over physObj (f dt)

runSolvers :: [PairSolver n Int a] -> n -> World a -> (World a, [PairSolver n Int a])
runSolvers ss dt ww = foldl f (ww, []) ss
  where f (w, ss') s = (w', s':ss')
          where (w', s') = runSolver s dt w

runSolver :: PairSolver n Int a -> n -> World a -> (World a, PairSolver n Int a)
runSolver s dt w = solveMany ijs s dt w
  where ijs = fmap pairIndex (allPairs w)

solveMany :: [(Int, Int)] -> PairSolver n Int a -> n -> World a -> (World a, PairSolver n Int a)
solveMany ijs s dt w = foldl f (w, s) ijs
  where f (w, s) ij = solveOne ij s dt w

solveOne :: (Int, Int) -> PairSolver n Int a -> n -> World a -> (World a, PairSolver n Int a)
solveOne ij = solve' (worldObjs . pairiix ij)

-- apply externals
-- apply solvers (until convergence requirements are met)
-- update position
updateWorld :: (Physical a n, Epsilon n, Floating n, Ord n) => n -> World a -> WorldBehavior n a -> (World a, WorldBehavior n a)
updateWorld dt w_ wb_ = f (wb_ ^. worldMaxIterations) (ww_, wb_) & _1 %~ advanceWorld dt
  where ww_ = foldl (\ww ext -> ww & worldObjs.traverse %~ ext dt) w_ (wb_ ^. worldExternals)
        f 0 wwb = wwb
        f n (w, wb) = if (wb ^. worldChanged) w (fst wwb') then f (n - 1) wwb'
                  else wwb'
          where wwb' = (w', wb & worldSolvers .~ ss')
                  where (w', ss') = runSolvers (wb ^. worldSolvers) dt w

getWorldChanged :: (Physical a n) => PhysObjChanged n -> WorldChanged a
getWorldChanged objChanged w w' = anyOf traverse id (ixZipWith f os os')
  where f o mo' = case mo' of
          Just o' -> objChanged (o ^. physObj) (o' ^. physObj)
          Nothing -> False
        os = w ^. worldObjs
        os' = w' ^. worldObjs

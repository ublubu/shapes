{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Physics.World.OptWorld where

import GHC.Generics (Generic)

import Control.DeepSeq
import Control.Lens
import Data.Foldable (foldl')
import qualified Data.IntMap.Strict as IM
import Physics.Constraint.OptConstraint hiding (solveConstraint)
import Utils.Utils

data World a = World { _worldObjs :: !(IM.IntMap a)
                     , _worldNextKey :: !Int } deriving (Show, Generic, NFData)
makeLenses ''World

emptyWorld :: World a
emptyWorld = World IM.empty 0

addObj :: World a -> a -> World a
addObj w o = w & worldObjs %~ IM.insert n o & worldNextKey .~ n + 1
  where n = w ^. worldNextKey

fromList :: [a] -> World a
fromList = foldl addObj emptyWorld

worldPair :: (Int, Int) -> Traversal' (World a) (a, a)
worldPair ij = worldObjs . pairiix ij

data WorldPair a = WorldPair (Int, Int) a deriving Show
type External' = Double -> PhysicalObj -> PhysicalObj
type External a = Double -> a -> a
type WorldChanged a = World a -> World a -> Bool

instance Functor WorldPair where
  fmap f (WorldPair ij x) = WorldPair ij (f x)

fromPair :: WorldPair a -> a
fromPair (WorldPair _ a) = a

pairIndex :: WorldPair a -> (Int, Int)
pairIndex (WorldPair ij _) = ij

advanceWorld :: (Physical a) => Double -> World a -> World a
advanceWorld dt w = w & worldObjs.traverse.physObj %~ (`advanceObj` dt)

allPairs :: World a -> [WorldPair (a, a)]
allPairs w = fst $ ifoldlOf (worldObjs.traversed) f ([], []) w
  where f i (pairs, xs) x = (foldl' g pairs xs, (i, x):xs)
          where g ps (j, x') = WorldPair (i, j) (x, x') : ps

allKeys :: World a -> [(Int, Int)]
allKeys = fmap pairIndex . allPairs

wrapExternal :: (Physical a) => External' -> External a
wrapExternal f dt = over physObj (f dt)

applyExternals :: [External a] -> Double -> World a -> World a
applyExternals exts dt w = foldl f w exts
  where f w0 ext = w0 & worldObjs.traverse %~ ext dt

getWorldChanged :: (Physical a) => PhysObjChanged -> WorldChanged a
getWorldChanged objChanged w w' = anyOf traverse id (ixZipWith f os os')
  where f o mo' = case mo' of
          Just o' -> objChanged (o ^. physObj) (o' ^. physObj)
          Nothing -> False
        os = w ^. worldObjs
        os' = w' ^. worldObjs

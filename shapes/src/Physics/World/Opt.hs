{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Physics.World.Opt where

import Control.Lens
import Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic.Mutable as MV

import Physics.Contact.Opt.ConvexHull
import Physics.Constraint.Opt hiding (solveConstraint)
import qualified Physics.Transform.Opt as PT
import Physics.World.Opt.Object

data World s =
  World { _wPhysObjs :: !(U.MVector s PhysicalObj)
        , _wShapes :: !(V.Vector ConvexHull)
        , _wMus :: !(U.Vector Double)
        }
makeLenses ''World

type External' = Double -> PhysicalObj -> PhysicalObj
type External a = Double -> a -> a

fromList :: [WorldObj] -> ST s (World s)
fromList objs = do
  physObjs <- U.thaw . U.fromList $ _worldPhysObj <$> objs
  let shapes = V.fromList $ _worldShape <$> objs
      mus = U.fromList $ _worldMu <$> objs

  return $ World physObjs shapes mus
{-# INLINE fromList #-}

updateShapes :: World s -> ST s (World s)
updateShapes w@World{..} = do
  shapes <- V.unsafeThaw _wShapes
  let f i = do
        obj <- MV.read _wPhysObjs i
        let t = _physObjTransform obj
        MV.modify shapes (`setHullTransform` PT.transform t) i
      {-# INLINE f #-}
  mapM_ f [0..(MV.length _wPhysObjs - 1)]
  shapes' <- V.unsafeFreeze shapes
  return $ w & wShapes .~ shapes'
{-# INLINE updateShapes #-}

advanceWorld :: Double -> World s -> ST s ()
advanceWorld dt =
  overPhysObjs (`advanceObj` dt)
{-# INLINE advanceWorld #-}

applyExternals :: [External PhysicalObj] -> Double -> World s -> ST s ()
applyExternals exts dt =
  overPhysObjs (\obj -> foldl f obj exts)
  where f obj ext = ext dt obj
        {-# INLINE f #-}
{-# INLINE applyExternals #-}

overPhysObjs :: (PhysicalObj -> PhysicalObj) -> World s -> ST s ()
overPhysObjs f = overEach f . view wPhysObjs
{-# INLINE overPhysObjs #-}

overPhysObjPair :: (Int, Int)
                -> ((PhysicalObj, PhysicalObj) -> (PhysicalObj, PhysicalObj))
                -> World s
                -> ST s ()
overPhysObjPair ij f =
  overPair ij f . view wPhysObjs
{-# INLINE overPhysObjPair #-}

overEach :: (MV.MVector v a) => (a -> a) -> v s a -> ST s ()
overEach f objs =
  mapM_ (MV.modify objs f) [0..(MV.length objs - 1)]
{-# INLINE overEach #-}

overPair :: (MV.MVector v a) => (Int, Int) -> ((a, a) -> (a, a)) -> v s a -> ST s ()
overPair (i, j) f objs = do
  a <- MV.read objs i
  b <- MV.read objs j
  let (a', b') = f (a, b)
  MV.write objs i a'
  MV.write objs j b'
{-# INLINE overPair #-}

setPair :: (MV.MVector v a) => (Int, Int) -> (a, a) -> v s a -> ST s ()
setPair (i, j) (a, b) objs = do
  MV.write objs i a
  MV.write objs j b
{-# INLINE setPair #-}

viewPair :: (MV.MVector v a) => (Int, Int) -> v s a -> ST s (a, a)
viewPair (i, j) objs = do
  a <- MV.read objs i
  b <- MV.read objs j
  return (a, b)
{-# INLINE viewPair #-}

viewPair' :: (U.Unbox a) => (Int, Int) -> U.Vector a -> (a, a)
viewPair' (i, j) objs = (objs U.! i, objs U.! j)
{-# INLINE viewPair' #-}

viewPair'' :: (Int, Int) -> V.Vector a -> (a, a)
viewPair'' (i, j) objs = (objs V.! i, objs V.! j)
{-# INLINE viewPair'' #-}

{-
worldPair :: (Int, Int) -> Traversal' (World a) (a, a)
worldPair ij = worldObjs . pairiix ij
{-# INLINE worldPair #-}

data WorldPair a = WorldPair (Int, Int) a deriving Show
type External' = Double -> PhysicalObj -> PhysicalObj
type External a = Double -> a -> a
type WorldChanged a = World a -> World a -> Bool

instance Functor WorldPair where
  fmap f (WorldPair ij x) = WorldPair ij (f x)
  {-# INLINE fmap #-}

fromPair :: WorldPair a -> a
fromPair (WorldPair _ a) = a
{-# INLINE fromPair #-}

pairIndex :: WorldPair a -> (Int, Int)
pairIndex (WorldPair ij _) = ij
{-# INLINE pairIndex #-}

advanceWorld :: (Physical a) => Double -> World a -> World a
advanceWorld dt w = w & worldObjs.traverse.physObj %~ (`advanceObj` dt)
{-# INLINE advanceWorld #-}

allPairs :: World a -> [WorldPair (a, a)]
allPairs w = fst $ ifoldlOf (worldObjs.traversed) f ([], []) w
  where f i (pairs, xs) x = (foldl' g pairs xs, (i, x):xs)
          where g ps (j, x') = WorldPair (i, j) (x, x') : ps
                {-# INLINE g #-}
        {-# INLINE f #-}
{-# INLINE allPairs #-}

allKeys :: World a -> [(Int, Int)]
allKeys = fmap pairIndex . allPairs
{-# INLINE allKeys #-}

wrapExternal :: (Physical a) => External' -> External a
wrapExternal f dt = over physObj (f dt)
{-# INLINE wrapExternal #-}

applyExternals :: [External a] -> Double -> World a -> World a
applyExternals exts dt w = foldl f w exts
  where f w0 ext = w0 & worldObjs.traverse %~ ext dt
        {-# INLINE f #-}
{-# INLINE applyExternals #-}

getWorldChanged :: (Physical a) => PhysObjChanged -> WorldChanged a
getWorldChanged objChanged w w' = anyOf traverse id (ixZipWith f os os')
  where f o mo' = case mo' of
          Just o' -> objChanged (o ^. physObj) (o' ^. physObj)
          Nothing -> False
        {-# INLINE f #-}
        os = w ^. worldObjs
        os' = w' ^. worldObjs
{-# INLINE getWorldChanged #-}
-}

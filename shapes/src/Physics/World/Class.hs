{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Physics.World.Class where

import Control.Lens hiding (transform)

import Physics.Constraint (PhysicalObj, advanceObj, _physObjTransform)
import Physics.Contact.ConvexHull (ConvexHull, setHullTransform)
import Physics.Transform (transform)

class Physical p where
  woPhys :: Functor f => (PhysicalObj -> f PhysicalObj) -> p -> f p

class (Physical p) => Contactable p where
  woMu :: Functor f => (Double -> f Double) -> p -> f p
  woShape :: Functor f => (ConvexHull -> f ConvexHull) -> p -> f p
  woMuShape :: Functor f
            => ((Double, ConvexHull) -> f (Double, ConvexHull))
            -> p
            -> f p

class (Ord k, Contactable o) => PhysicsWorld k w o | w -> k o where
  wKeys :: w -> [k]
  wObj :: k -> Traversal' w o
  wPair :: (k, k) -> Traversal' w (o, o)
  wObjs :: IndexedTraversal' k w o -- in ascending k order

wAdvance :: (PhysicsWorld k w o) => Double -> w -> w
wAdvance dt w = w & wObjs.woPhys %~ (`advanceObj` dt)
{-# INLINE wAdvance #-}

woUpdateShape :: (Contactable o) => o -> o
woUpdateShape obj =
  obj & woShape %~ flip setHullTransform (transform t)
  where t = _physObjTransform . view woPhys $ obj
{-# INLINE woUpdateShape #-}

type External = Double -> PhysicalObj -> PhysicalObj

wApplyExternals :: (PhysicsWorld k w o) => [External] -> Double -> w -> w
wApplyExternals exts dt w = foldl f w exts
  where f w0 ext = w0 & wObjs.woPhys %~ ext dt
        {-# INLINE f #-}
{-# INLINE wApplyExternals #-}

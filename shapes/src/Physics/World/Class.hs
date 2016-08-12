{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Physics.World.Class where

import Control.Lens hiding (transform)

import Physics.Constraint (PhysicalObj, advanceObj, _physObjTransform)
import Physics.Contact.ConvexHull (ConvexHull, setHullTransform)
import Physics.Transform (transform)

-- | Class for objects with physical properties.
class Physical p where
  -- | Lens for the embedded 'PhysicalObj'
  woPhys :: Functor f => (PhysicalObj -> f PhysicalObj) -> p -> f p

-- | Class for objects that can be in contact with each other.
class (Physical p) => Contactable p where
  -- | Lens for embedded coefficient of friction \"mu\"
  woMu :: Functor f => (Double -> f Double) -> p -> f p
  -- | Lens for embedded contact shape
  woShape :: Functor f => (ConvexHull -> f ConvexHull) -> p -> f p
  -- | Lens for embedded pair of (coefficient of friction, contact shape)
  woMuShape :: Functor f
            => ((Double, ConvexHull) -> f (Double, ConvexHull))
            -> p
            -> f p

-- | Class for worlds (:: w) inhabited by physical objects (:: o)
-- each uniquely identified by a key (:: k)
class (Ord k, Contactable o) => PhysicsWorld k w o | w -> k o where
  -- | Keys of all the world's inhabitants
  wKeys :: w -> [k]
  -- | 'Traversal' of inhabitants with a given key
  wObj :: k -> Traversal' w o
  -- | 'Traversal'' of pairs of inhabitants with a given pair of keys
  wPair :: (k, k) -> Traversal' w (o, o)
  -- | 'IndexedTraversal'' of all inhabitants
  wObjs :: IndexedTraversal' k w o

-- | Advance the physical state of the world by a given time delta
-- using each inhabitant's current velocity.
wAdvance :: (PhysicsWorld k w o) => Double -- ^ Time delta
         -> w
         -> w
wAdvance dt w = w & wObjs.woPhys %~ (`advanceObj` dt)
{-# INLINE wAdvance #-}

-- | Update the shape of an object to match its current physical state.
--
-- By keeping all shapes in world space, we ensure that each shape
-- only needs to be transformed once per frame.
woUpdateShape :: (Contactable o) => o -> o
woUpdateShape obj =
  obj & woShape %~ flip setHullTransform (transform t)
  where t = _physObjTransform . view woPhys $ obj
{-# INLINE woUpdateShape #-}

-- | Non-constraint effects (e.g. gravity) on physical objects
type External = Double -> PhysicalObj -> PhysicalObj

-- | Apply 'External' effects to the objects in a world.
--
-- This happens each frame before constraints are created and solved.
wApplyExternals :: (PhysicsWorld k w o) => [External] -> Double -> w -> w
wApplyExternals exts dt w = foldl f w exts
  where f w0 ext = w0 & wObjs.woPhys %~ ext dt
        {-# INLINE f #-}
{-# INLINE wApplyExternals #-}

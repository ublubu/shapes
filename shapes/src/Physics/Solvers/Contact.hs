{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Physics.Solvers.Contact where

import Control.Lens
import Control.Monad
import Control.Monad.ST
import Data.Maybe
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Generic.Mutable as MV

import Physics.Constraint
import Physics.Constraints.Contact
import Physics.Constraints.Types
import Physics.Contact
import Physics.World.Class
import Utils.Descending
import Utils.Utils

prepareFrame :: (PhysicsWorld k w o)
             => Descending (k, k)
             -> w
             -> Descending (ObjectFeatureKey k, Flipping Contact')
prepareFrame pairKeys w =
  join $ f <$> pairKeys
  where f pairKey = keyedContacts pairKey shapes
          where shapes = pairMap (view woShape) $ fromJust (w ^? wPair pairKey)
        {-# INLINE f #-}
{-# INLINE prepareFrame #-}

applySln :: ContactResult Lagrangian
         -> ContactResult Constraint
         -> (PhysicalObj, PhysicalObj)
         -> (PhysicalObj, PhysicalObj)
applySln crL crConstraint ab =
  foldl (flip ($)) ab $ applyLagrangian <$> crL <*> crConstraint
{-# INLINE applySln #-}

-- Calculate all new constraints from the contacts.
-- Apply cached lagrangians using new constraints.
-- Build new lagrangians cache with either zero or previously cached value.
-- TODO: reader monad for stuff that's const between frames (beh, dt)
applyCachedSlns :: forall s k w o. (V.Unbox k, PhysicsWorld k w o)
                => ContactBehavior
                -> Double
                -> Descending (ObjectFeatureKey k, Flipping Contact')
                -> V.MVector s (ObjectFeatureKey k, ContactResult Lagrangian)
                -> w
                -> ST s ( V.MVector s (ObjectFeatureKey k, ContactResult Lagrangian)
                        , V.Vector (ContactResult Constraint)
                        , w
                        )
applyCachedSlns beh dt kContacts oldLagrangians world0 = do
  lagrangians <- MV.new contactCount
  constraints <- MV.new contactCount

  let newCache :: (Int, w)
               -> (ObjectFeatureKey k, Flipping Contact')
               -> ST s (Int, w)
      newCache (cache_i', world) (key@ObjectFeatureKey{..}, fContact) = do
        let ab = fromJust $ iixView (\k -> wObj k . woPhys) _ofkObjKeys world
            constraint = constraintGen beh dt fContact ab
        MV.write lagrangians cache_i' (key, pure 0)
        MV.write constraints cache_i' constraint
        return (cache_i' + 1, world)
      {-# INLINE newCache #-}

      useCache :: (Int, w)
               -> (ObjectFeatureKey k, Flipping Contact')
               -> (ObjectFeatureKey k, ContactResult Lagrangian)
               -> ST s (Int, w)
      useCache (cache_i', world) (ObjectFeatureKey{..}, fContact) kLagr@(_, lagr) = do
        let ab = fromJust $ iixView (\k -> wObj k . woPhys) _ofkObjKeys world
            constraint = constraintGen beh dt fContact ab
            world' = iixOver (\k -> wObj k . woPhys) (applySln lagr constraint)
              _ofkObjKeys world
        MV.write lagrangians cache_i' kLagr
        MV.write constraints cache_i' constraint
        return (cache_i' + 1, world')
      {-# INLINE useCache #-}

  (_, world1) <- descZipVector fst fst useCache newCache (0, world0) kContacts oldLagrangians
  frozenConstraints <- V.unsafeFreeze constraints

  return (lagrangians, frozenConstraints, world1)
  where contactCount = length kContacts
{-# INLINE applyCachedSlns #-}

improveSln :: (V.Unbox k, Contactable o)
           => SolutionProcessor (Double, Double) (ContactResult Lagrangian)
           -> ObjectFeatureKey k
           -> Int
           -> V.MVector s (ObjectFeatureKey k, ContactResult Lagrangian)
           -> V.Vector (ContactResult Constraint)
           -> (o, o)
           -> ST s (o, o)
improveSln slnProc key cache_i lagrangians constraints ab = do
  (_, cached_l) <- MV.read lagrangians cache_i
  let constraint = constraints V.! cache_i
      phys_ab = pairView woPhys ab
      mu_ab = pairView woMu ab
      new_l = lagrangian2 phys_ab <$> constraint
      processed_l = slnProc mu_ab cached_l new_l
      phys_ab' = applySln (_processedToApply processed_l) constraint phys_ab
  MV.write lagrangians cache_i (key, _processedToCache processed_l)
  return $ pairSet woPhys phys_ab' ab
{-# INLINE improveSln #-}

improveWorld' :: (V.Unbox k, PhysicsWorld k w o)
              => SolutionProcessor (Double, Double) (ContactResult Lagrangian)
              -> ObjectFeatureKey k
              -> Int
              -> V.MVector s (ObjectFeatureKey k, ContactResult Lagrangian)
              -> V.Vector (ContactResult Constraint)
              -> w
              -> ST s w
improveWorld' slnProc key@ObjectFeatureKey{..} cache_i lagrangians constraints =
  iixOver' wObj f _ofkObjKeys
  where f = improveSln slnProc key cache_i lagrangians constraints
{-# INLINE improveWorld' #-}

improveWorld :: (V.Unbox k, PhysicsWorld k w o)
             => SolutionProcessor (Double, Double) (ContactResult Lagrangian)
             -> Descending (ObjectFeatureKey k, Flipping Contact')
             -> V.MVector s (ObjectFeatureKey k, ContactResult Lagrangian)
             -> V.Vector (ContactResult Constraint)
             -> w
             -> ST s w
improveWorld slnProc kContacts lagrangians constraints world0 =
  snd <$> foldM f (0, world0) kContacts
  where f (cache_i, world) (key, _) =
          (,) (cache_i + 1) <$> improveWorld' slnProc key cache_i lagrangians constraints world
{-# INLINE improveWorld #-}

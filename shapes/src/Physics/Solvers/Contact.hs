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
import Physics.World
import Utils.Descending
import Utils.Utils

prepareFrame :: (Contactable a)
             => Descending (Int, Int)
             -> World a
             -> Descending (ObjectFeatureKey, Flipping Contact')
prepareFrame pairKeys w =
  join $ f <$> pairKeys
  where f pairKey = keyedContacts pairKey (fromJust $ w ^? worldPair pairKey)
        {-# INLINE f #-}
{-# INLINE prepareFrame #-}

applySln :: (Contactable a)
         => ContactResult Lagrangian
         -> ContactResult Constraint
         -> (a, a)
         -> (a, a)
applySln crL crConstraint ab =
  foldl (flip ($)) ab $ applyLagrangian <$> crL <*> crConstraint
{-# INLINE applySln #-}

--TODO: reader monad for stuff that's const between frames (beh, dt)
applyCachedSlns :: forall s a. (Contactable a)
                => ContactBehavior
                -> Double
                -> Descending (ObjectFeatureKey, Flipping Contact')
                -> V.MVector s (ObjectFeatureKey, ContactResult Lagrangian)
                -> World a
                -> ST s ( V.MVector s (ObjectFeatureKey, ContactResult Lagrangian)
                        , V.Vector (ContactResult Constraint)
                        , World a
                        )
applyCachedSlns beh dt kContacts oldLagrangians world0 = do
  lagrangians <- MV.new contactCount
  constraints <- MV.new contactCount

  let newCache :: (Int, World a)
               -> (ObjectFeatureKey, Flipping Contact')
               -> ST s (Int, World a)
      newCache (cache_i', world) (key@ObjectFeatureKey{..}, fContact) = do
        let ab = fromJust $ world ^? worldPair (fromSP _ofkObjKeys)
            constraint = constraintGen beh dt fContact ab
        MV.write lagrangians cache_i' (key, pure 0)
        MV.write constraints cache_i' constraint
        return (cache_i' + 1, world)
      {-# INLINE newCache #-}

      useCache :: (Int, World a)
               -> (ObjectFeatureKey, Flipping Contact')
               -> (ObjectFeatureKey, ContactResult Lagrangian)
               -> ST s (Int, World a)
      useCache (cache_i', world) (ObjectFeatureKey{..}, fContact) (_, lagr) = do
        let ab = fromJust $ world ^? worldPair (fromSP _ofkObjKeys)
            constraint = constraintGen beh dt fContact ab
            world' = world & worldPair (fromSP _ofkObjKeys) %~ applySln lagr constraint
        MV.write constraints cache_i' constraint
        return (cache_i' + 1, world')
      {-# INLINE useCache #-}

  (_, world1) <- descZipVector fst fst useCache newCache (0, world0) kContacts oldLagrangians
  frozenConstraints <- V.unsafeFreeze constraints

  return (lagrangians, frozenConstraints, world1)
  where contactCount = length kContacts
{-# INLINE applyCachedSlns #-}

improveSln :: (Contactable a)
            => SolutionProcessor a (ContactResult Lagrangian)
            -> ObjectFeatureKey
            -> Int
            -> V.MVector s (ObjectFeatureKey, ContactResult Lagrangian)
            -> V.Vector (ContactResult Constraint)
            -> (a, a)
            -> ST s (a, a)
improveSln slnProc key cache_i lagrangians constraints ab = do
  (_, cached_l) <- MV.read lagrangians cache_i
  let constraint = constraints V.! cache_i
      new_l = lagrangian2 ab <$> constraint
      processed_l = slnProc ab cached_l new_l
  MV.write lagrangians cache_i (key, _processedToCache processed_l)
  return $ applySln (_processedToApply processed_l) constraint ab
{-# INLINE improveSln #-}

improveWorld' :: (Contactable a)
              => SolutionProcessor a (ContactResult Lagrangian)
              -> ObjectFeatureKey
              -> Int
              -> V.MVector s (ObjectFeatureKey, ContactResult Lagrangian)
              -> V.Vector (ContactResult Constraint)
              -> World a
              -> ST s (World a)
improveWorld' slnProc key@ObjectFeatureKey{..} cache_i lagrangians constraints =
  worldPair (fromSP _ofkObjKeys) f
  where f = improveSln slnProc key cache_i lagrangians constraints
{-# INLINE improveWorld' #-}

improveWorld :: (Contactable a)
             => SolutionProcessor a (ContactResult Lagrangian)
             -> Descending (ObjectFeatureKey, Flipping Contact')
             -> V.MVector s (ObjectFeatureKey, ContactResult Lagrangian)
             -> V.Vector (ContactResult Constraint)
             -> World a
             -> ST s (World a)
improveWorld slnProc kContacts lagrangians constraints world0 =
  snd <$> foldM f (0, world0) kContacts
  where f (cache_i, world) (key, _) =
          (,) (cache_i + 1) <$> improveWorld' slnProc key cache_i lagrangians constraints world
{-# INLINE improveWorld #-}

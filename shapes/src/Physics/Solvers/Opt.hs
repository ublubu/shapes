{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Physics.Solvers.Opt where

import Control.Lens
import Control.Monad
import Control.Monad.ST
import Data.Maybe
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Generic.Mutable as MV

import Physics.Constraint.Opt
import Physics.Constraints.Opt.Contact
import Physics.Contact.Opt
import Physics.Solvers.Opt.SolutionProcessors
import Physics.World.Opt
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
         => ContactSolution
         -> (a, a)
         -> (a, a)
applySln ContactSolution{..} =
  applyConstraintResult _contactFriction . applyConstraintResult _contactNonPen
{-# INLINE applySln #-}

--TODO: reader monad for stuff that's const between frames (beh, dt)
applyCachedSlns :: forall s a. (Contactable a)
                => SolutionProcessor a
                -> ContactBehavior
                -> Double
                -> Descending (ObjectFeatureKey, Flipping Contact')
                -> V.MVector s (ObjectFeatureKey, ContactSolution)
                -> World a
                -> ST s (V.MVector s (ObjectFeatureKey, ContactSolution), World a)
applyCachedSlns slnProc beh dt kContacts cache world0 = do
  cache' <- MV.new $ length kContacts

  let newCache :: (Int, World a)
               -> (ObjectFeatureKey, Flipping Contact')
               -> ST s (Int, World a)
      newCache (cache_i', world) (key@ObjectFeatureKey{..}, fContact) = do
        let ab = fromJust $ world ^? worldPair (fromSP _ofkObjKeys)
            sln = getContactConstraint beh dt ab fContact
        MV.write cache' cache_i' (key, sln)
        return (cache_i' + 1, world)
      {-# INLINE newCache #-}

      useCache :: (Int, World a)
               -> (ObjectFeatureKey, Flipping Contact')
               -> (ObjectFeatureKey, ContactSolution)
               -> ST s (Int, World a)
      useCache (cache_i', world) (key@ObjectFeatureKey{..}, fContact) (_, sln) = do
        let ab = fromJust $ world ^? worldPair (fromSP _ofkObjKeys)
            sln' = updateContactSln beh dt sln ab fContact
            world' = world & worldPair (fromSP _ofkObjKeys) %~ applySln sln'
        MV.write cache' cache_i' (key, sln')
        return (cache_i' + 1, world')
      {-# INLINE useCache #-}

  (_, world1) <- descZipVector fst fst useCache newCache (0, world0) kContacts cache
  return (cache', world1)
{-# INLINE applyCachedSlns #-}

improveSln :: (Contactable a)
            => SolutionProcessor a
            -> ObjectFeatureKey
            -> Int
            -> V.MVector s (ObjectFeatureKey, ContactSolution)
            -> (a, a)
            -> ST s (a, a)
improveSln slnProc key cache_i cache ab = do
  (_, cachedSln) <- MV.read cache cache_i
  let sln = solveContactAgain cachedSln ab
      (slnCache, slnApply) = slnProc cachedSln sln ab
  MV.write cache cache_i (key, slnCache)
  return $ applySln slnApply ab
{-# INLINE improveSln #-}

improveWorld' :: (Contactable a)
              => SolutionProcessor a
              -> ObjectFeatureKey
              -> Int
              -> V.MVector s (ObjectFeatureKey, ContactSolution)
              -> World a
              -> ST s (World a)
improveWorld' slnProc key@ObjectFeatureKey{..} cache_i cache =
  worldPair (fromSP _ofkObjKeys) f
  where f = improveSln slnProc key cache_i cache
{-# INLINE improveWorld' #-}

improveWorld :: (Contactable a)
             => SolutionProcessor a
             -> Descending (ObjectFeatureKey, Flipping Contact')
             -> V.MVector s (ObjectFeatureKey, ContactSolution)
             -> World a
             -> ST s (World a)
improveWorld slnProc kContacts cache world0 =
  snd <$> foldM f (0, world0) kContacts
  where f (cache_i, world) (key, _) =
          (,) (cache_i + 1) <$> improveWorld' slnProc key cache_i cache world
{-# INLINE improveWorld #-}

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
import Utils.Utils

prepareFrame :: (Contactable a)
             => Descending (Int, Int)
             -> World a
             -> Descending (ObjectFeatureKey, Flipping Contact')
prepareFrame pairKeys w =
  join $ f <$> pairKeys
  where f pairKey = keyedContacts pairKey (fromJust $ w ^? worldPair pairKey)

applySln :: (Contactable a)
         => ContactSolution
         -> (a, a)
         -> (a, a)
applySln ContactSolution{..} =
  applyConstraintResult _contactFriction . applyConstraintResult _contactNonPen

--TODO: reader monad for stuff that's const between frames (beh, dt)
applyCachedSlns :: forall s a. (Contactable a)
                => ContactBehavior
                -> Double
                -> Descending (ObjectFeatureKey, Flipping Contact')
                -> V.MVector s (ObjectFeatureKey, ContactSolution)
                -> World a
                -> ST s (V.MVector s (ObjectFeatureKey, ContactSolution), World a)
applyCachedSlns beh dt (Descending kContacts) cache world0 = do
  cache' <- MV.new keyCount
  let f :: (Int, Int, World a)
        -> (ObjectFeatureKey, Flipping Contact')
        -> ST s (Int, Int, World a)
      f (cache_i, cache_i', world) kc@(key@ObjectFeatureKey{..}, _)
        | cache_i < cacheCount = do
            (key', sln) <- MV.read cache cache_i
            if key < key'
              then f (cache_i + 1, cache_i', world) kc -- keep looking
              else if key == key'
                   then h' cache_i cache_i' world kc sln
                   else g' cache_i cache_i' world kc
        | otherwise = g' cache_i cache_i' world kc
      g :: Int -> World a -> ObjectFeatureKey -> Flipping Contact' -> ST s (World a)
      g cache_i' world key@ObjectFeatureKey{..} fContact = do
        let ab = fromJust $ world ^? worldPair (fromSP _ofkObjKeys)
            sln = solveContact beh dt ab fContact
        MV.write cache' cache_i' (key, sln)
        return $ world & worldPair (fromSP _ofkObjKeys) %~ applySln sln
      g' cache_i cache_i' world (key@ObjectFeatureKey{..}, fContact) = do
        world' <- g cache_i' world key fContact
        return (cache_i, cache_i' + 1, world')
      h cache_i' world key@ObjectFeatureKey{..} fContact sln = do
        let ab = fromJust $ world ^? worldPair (fromSP _ofkObjKeys)
            sln' = updateContactSln beh dt sln ab fContact
        MV.write cache' cache_i' (key, sln')
        return $ world & worldPair (fromSP _ofkObjKeys) %~ applySln sln
      h' cache_i cache_i' world (key@ObjectFeatureKey{..}, fContact) sln = do
        world' <- h cache_i' world key fContact sln
        return (cache_i + 1, cache_i' + 1, world')
  (_, _, world1) <- foldM f (0, 0, world0) kContacts
  return (cache', world1)
  where keyCount = length kContacts
        cacheCount = MV.length cache

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

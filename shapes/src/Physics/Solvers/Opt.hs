{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Physics.Solvers.Opt where

import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Generic.Mutable as MV

import Physics.Constraint.Opt
import Physics.Constraints.Opt.Contact
import Physics.Contact.Opt
import Physics.Solvers.Opt.SolutionProcessors
import Physics.World.Opt
import Utils.Descending
import Utils.Utils

prepareFrame :: Descending (Int, Int)
             -> World s
             -> Descending (ObjectFeatureKey, Flipping Contact')
prepareFrame pairKeys World{..} =
  join $ f <$> pairKeys
  where f pairKey = keyedContacts pairKey $ viewPair'' pairKey _wShapes
--{-# INLINE prepareFrame #-}

applySln :: ContactSolution
         -> (PhysicalObj, PhysicalObj)
         -> (PhysicalObj, PhysicalObj)
applySln ContactSolution{..} =
  applyConstraintResult _contactFriction . applyConstraintResult _contactNonPen
{-# INLINE applySln #-}

--TODO: reader monad for stuff that's const between frames (beh, dt)
applyCachedSlns :: forall s. ContactBehavior
                -> Double
                -> Descending (ObjectFeatureKey, Flipping Contact')
                -> V.MVector s (ObjectFeatureKey, ContactSolution)
                -> World s
                -> ST s (V.MVector s (ObjectFeatureKey, ContactSolution))
applyCachedSlns beh dt kContacts cache World{..} = do
  cache' <- MV.new $ length kContacts

  let newCache :: Int
               -> (ObjectFeatureKey, Flipping Contact')
               -> ST s Int
      newCache cache_i' (key@ObjectFeatureKey{..}, fContact) = do
        ab <- viewPair (fromSP _ofkObjKeys) _wPhysObjs
        let sln = getContactConstraint beh dt ab fContact
        MV.write cache' cache_i' (key, sln)
        return $ cache_i' + 1

      useCache :: Int
               -> (ObjectFeatureKey, Flipping Contact')
               -> (ObjectFeatureKey, ContactSolution)
               -> ST s Int
      useCache cache_i' (key@ObjectFeatureKey{..}, fContact) (_, sln) = do
        let objKeys = fromSP _ofkObjKeys
        ab <- viewPair objKeys _wPhysObjs
        let sln' = updateContactSln beh dt sln ab fContact

        overPair objKeys (applySln sln') _wPhysObjs
        MV.write cache' cache_i' (key, sln')
        return $ cache_i' + 1

  void $ descZipVector fst fst useCache newCache 0 kContacts cache
  return cache'
--{-# INLINE applyCachedSlns #-}

improveSln :: SolutionProcessor
           -> ObjectFeatureKey
           -> Int
           -> V.MVector s (ObjectFeatureKey, ContactSolution)
           -> World s
           -> ST s ()
improveSln slnProc key@ObjectFeatureKey{..} cache_i cache World{..} = do
  (_, cachedSln) <- MV.read cache cache_i
  ab <- viewPair objKeys _wPhysObjs
  let mu_ab = viewPair' objKeys _wMus
      sln = solveContactAgain cachedSln ab
      (slnCache, slnApply) = slnProc cachedSln sln mu_ab
  MV.write cache cache_i (key, slnCache)
  setPair objKeys (applySln slnApply ab) _wPhysObjs
  where objKeys = fromSP _ofkObjKeys
{-# INLINE improveSln #-}

-- inlining this fn causes GHC panic: simplifier ticks exhausted
improveWorld :: forall s. SolutionProcessor
             -> Descending (ObjectFeatureKey, Flipping Contact')
             -> V.MVector s (ObjectFeatureKey, ContactSolution)
             -> World s
             -> ST s ()
improveWorld slnProc kContacts cache world =
  void $ foldM f 0 kContacts
  where f :: Int -> (ObjectFeatureKey, a) -> ST s Int
        f cache_i (key, _) = do
          improveSln slnProc key cache_i cache world
          return $ cache_i + 1

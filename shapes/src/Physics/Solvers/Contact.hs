{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{- |
This is the backbone of the physics engine.
The functions here find contacts between objects and generate and solve constraints for these contacts.
It exploits temporal coherence of the scene by caching constraint solutions between frames.
This way, it can accumulate stability over time instead of requiring many solver iterations each frame.

The functions in this module are designed to be used in this order:

1. 'prepareFrame' - Which contacts are creating constraints for us to solve this frame?
2. 'applyCachedSlns' - Build this frame's Lagrangian and constraint caches. Apply relevant Lagrangians from the previous frame.
3. 'improveWorld' - Iteratively solve the constraints and update the cached Lagrangians. (Can do this step multiple times.)

The cache of Lagrangians should be retained for the next frame's 'applyCachedSlns'.
-}
module Physics.Solvers.Contact where

import           Control.Lens
import           Control.Monad
import           Control.Monad.ST
import           Data.Maybe
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Vector.Unboxed         as V

import           Physics.Constraint
import           Physics.Constraints.Contact
import           Physics.Constraints.Types
import           Physics.Contact.Types
import           Physics.World
import           Physics.World.Object
import           Utils.Descending
import           Utils.Utils

-- | Calculate all contacts for the current frame.
prepareFrame ::
     Descending (Int, Int) -- ^ broadphase-filtered pairs of shapes to check for contact
  -> World usr -- ^ the world
  -> Descending (ObjectFeatureKey Int, Flipping Contact') -- ^ list of contacts between shapes (in descending order of 'ObjectFeatureKey' because the caches are ordered)
prepareFrame pairKeys w =
  join $ f <$> pairKeys
  where f pairKey = keyedContacts pairKey shapes
          where shapes = pairMap (view woShape) $ fromJust (w ^? wPair pairKey)
        {-# INLINE f #-}
{-# INLINE prepareFrame #-}

-- | Update a pair of shapes based on the solution to their constraint.
applySln ::
     ContactResult Lagrangian -- ^ the solution
  -> ContactResult Constraint -- ^ the constraint
  -> (PhysicalObj, PhysicalObj)
  -> (PhysicalObj, PhysicalObj)
applySln crL crConstraint ab =
  foldl (flip ($)) ab $ applyLagrangian <$> crL <*> crConstraint
{-# INLINE applySln #-}

{- |
Calculate all new constraints from the contacts.
Apply cached lagrangians using new constraints.
Build new lagrangians cache with either zero or previously cached value.

TODO: reader monad for stuff that's const between frames (beh, dt)
-}
applyCachedSlns ::
     forall s usr.
     ContactBehavior
  -> Double -- ^ dt
  -> Descending (ObjectFeatureKey Int, Flipping Contact') -- ^ list of contacts between shapes
  -> V.MVector s (ObjectFeatureKey Int, ContactResult Lagrangian) -- ^ list of constraint solutions from the previous frame
  -> World usr -- ^ the world
  -> ST s ( V.MVector s (ObjectFeatureKey Int, ContactResult Lagrangian)
          , V.Vector (ContactResult Constraint)
          , World usr)
                          -- ^ (this frame's constraint solutions, this frame's constraints, the updated world)
applyCachedSlns beh dt kContacts oldLagrangians world0 = do
  lagrangians <- MV.new contactCount
  constraints <- MV.new contactCount
  let newCache ::
           (Int, World usr) -- ^ (current index in cache, current world)
        -> (ObjectFeatureKey Int, Flipping Contact') -- ^ the contact to store at this index in the cache
        -> ST s (Int, World usr) -- ^ (next index in cache, updated world)
      newCache (cache_i', world) (key@ObjectFeatureKey {..}, fContact) = do
        let ab = fromJust $ iixView (\k -> wObj k . woPhys) _ofkObjKeys world
            -- ^ a pair of shapes (a, b)
            constraint = constraintGen beh dt fContact ab
        -- no previously-cached lagrangian, so start with 0.
        MV.write lagrangians cache_i' (key, pure 0)
        -- save the constraint so we can solve it (calculate/apply lagrangian)
        MV.write constraints cache_i' constraint
        return (cache_i' + 1, world)
      {-# INLINE newCache #-}
      useCache ::
           (Int, World usr) -- ^ (current index in cache, current world)
        -> (ObjectFeatureKey Int, Flipping Contact') -- ^ the contact to store at this index in the cache
        -> (ObjectFeatureKey Int, ContactResult Lagrangian) -- ^ the previous frame's solution for the last frame's corresponding contact
        -> ST s (Int, World usr) -- ^ (next index in cache, updated world)
      useCache (cache_i', world) (ObjectFeatureKey {..}, fContact) kLagr@(_, lagr) = do
        let ab = fromJust $ iixView (\k -> wObj k . woPhys) _ofkObjKeys world
            -- ^ a pair of shapes (a, b)
            constraint = constraintGen beh dt fContact ab
            world' =
              iixOver
                (\k -> wObj k . woPhys)
                (applySln lagr constraint)
                _ofkObjKeys
                world
            -- ^ update the world by applying the cached lagrangian with the newly-calculated constraint
        -- propagate the previously-cached lagrangian to the current frame's cache
        MV.write lagrangians cache_i' kLagr
        -- save the constraint so we can solve it (calculate/apply lagrangian)
        MV.write constraints cache_i' constraint
        return (cache_i' + 1, world')
      {-# INLINE useCache #-}
  -- zip the previous frame's cached solutions into this frame's contacts, applying cached solutions as we go
  (_, world1) <-
    descZipVector fst fst useCache newCache (0, world0) kContacts oldLagrangians
  frozenConstraints <- V.unsafeFreeze constraints
  return (lagrangians, frozenConstraints, world1)
  where
    contactCount = length kContacts
{-# INLINE applyCachedSlns #-}

-- | Solve the constraints for a given contact. (And apply the solution.)
improveSln ::
  SolutionProcessor (Double, Double) (ContactResult Lagrangian)
  -> ObjectFeatureKey Int -- ^ identifies the contact: which objects, and which features within the objects
  -> Int -- ^ index in the solution/constraint caches
  -> V.MVector s (ObjectFeatureKey Int, ContactResult Lagrangian) -- ^ solution cache
  -> V.Vector (ContactResult Constraint) -- ^ constraint cache
  -> (WorldObj usr, WorldObj usr) -- ^ pair of objects
  -> ST s (WorldObj usr, WorldObj usr) -- ^ updated pair of objects
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

-- | Wraps `improveSln` to operate on the world instead of a pair of objects.
improveWorld' ::
  SolutionProcessor (Double, Double) (ContactResult Lagrangian)
  -> ObjectFeatureKey Int
  -> Int
  -> V.MVector s (ObjectFeatureKey Int, ContactResult Lagrangian)
  -> V.Vector (ContactResult Constraint)
  -> World usr
  -> ST s (World usr)
improveWorld' slnProc key@ObjectFeatureKey{..} cache_i lagrangians constraints =
  iixOver' wObj f _ofkObjKeys
  where f = improveSln slnProc key cache_i lagrangians constraints
{-# INLINE improveWorld' #-}

-- | Run `improveSln` on every constraint in the world.
improveWorld ::
  SolutionProcessor (Double, Double) (ContactResult Lagrangian)
  -> Descending (ObjectFeatureKey Int, Flipping Contact')
  -> V.MVector s (ObjectFeatureKey Int, ContactResult Lagrangian)
  -> V.Vector (ContactResult Constraint)
  -> World usr
  -> ST s (World usr)
improveWorld slnProc kContacts lagrangians constraints world0 =
  snd <$> foldM f (0, world0) kContacts
  where f (cache_i, world) (key, _) =
          (,) (cache_i + 1) <$> improveWorld' slnProc key cache_i lagrangians constraints world
{-# INLINE improveWorld #-}

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
import           Control.Monad.Primitive
import           Data.Maybe
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Vector.Unboxed         as V

import           Physics.Constraint
import           Physics.Constraints.Contact
import           Physics.Constraints.Types
import           Physics.Contact.Types
import           Physics.World
import           Utils.Descending
import           Utils.Utils

-- | Calculate all contacts for the current frame.
prepareFrame ::
     (PrimMonad m)
  => Descending (Int, Int) -- ^ broadphase-filtered pairs of shapes to check for contact
  -> World (PrimState m) label -- ^ the world
  -> m (Descending (ObjectFeatureKey Int, Flipping Contact')) -- ^ list of contacts between shapes (in descending order of 'ObjectFeatureKey' because the caches are ordered)
prepareFrame (Descending pairKeys) world = do
  contacts <- mapM findContacts pairKeys
  return . Descending $ join contacts
  where
    findContacts (i, j) = do
      shape_i <- readShape world i
      shape_j <- readShape world j
      return . _descList $ keyedContacts (i, j) (shape_i, shape_j)

-- | Update a pair of shapes based on the solution to their constraint.
applySln ::
     ContactResult Lagrangian -- ^ the solution
  -> ContactResult Constraint -- ^ the constraint
  -> (PhysicalObj, PhysicalObj)
  -> (PhysicalObj, PhysicalObj)
applySln crL crConstraint ab =
  foldl (flip ($)) ab $ applyLagrangian <$> crL <*> crConstraint

{- |
Calculate all new constraints from the contacts.
Apply cached lagrangians using new constraints.
Build new lagrangians cache with either zero or previously cached value.

TODO: reader monad for stuff that's const between frames (beh, dt)
-}
applyCachedSlns ::
     forall m label.
     (PrimMonad m)
  => ContactBehavior
  -> Double -- ^ dt
  -> Descending (ObjectFeatureKey Int, Flipping Contact') -- ^ list of contacts between shapes
  -> V.MVector (PrimState m) (ObjectFeatureKey Int, ContactResult Lagrangian) -- ^ list of constraint solutions from the previous frame
  -> World (PrimState m) label -- ^ the world
  -> m ( V.MVector (PrimState m) (ObjectFeatureKey Int, ContactResult Lagrangian)
          , V.Vector (ContactResult Constraint) -- ^ (this frame's constraint solutions, this frame's constraints)
           )
applyCachedSlns beh dt kContacts oldLagrangians world = do
  lagrangians <- MV.new contactCount
  constraints <- MV.new contactCount
  let newCache ::
           Int -- ^ current index in cache
        -> (ObjectFeatureKey Int, Flipping Contact') -- ^ the contact to store at this index in the cache
        -> m Int -- ^ next index in cache
      newCache i (key@ObjectFeatureKey {..}, fContact) = do
        ab <- readPhysObjPair world _ofkObjKeys
        let constraint = constraintGen beh dt fContact ab
        -- no previously-cached lagrangian, so start with 0.
        MV.write lagrangians i (key, pure 0)
        -- save the constraint so we can solve it (calculate/apply lagrangian)
        MV.write constraints i constraint
        return (i + 1)
      useCache ::
           Int -- ^ current index in cache
        -> (ObjectFeatureKey Int, Flipping Contact') -- ^ the contact to store at this index in the cache
        -> (ObjectFeatureKey Int, ContactResult Lagrangian) -- ^ the previous frame's solution for the last frame's corresponding contact
        -> m Int -- ^ next index in cache
      useCache i (ObjectFeatureKey {..}, fContact) kLagr@(_, lagr) = do
        ab <- readPhysObjPair world _ofkObjKeys
        let constraint = constraintGen beh dt fContact ab
        modifyPhysObjPair world (applySln lagr constraint) _ofkObjKeys
        -- propagate the previously-cached lagrangian to the current frame's cache
        MV.write lagrangians i kLagr
        -- save the constraint so we can solve it (calculate/apply lagrangian)
        MV.write constraints i constraint
        return (i + 1)
  -- zip the previous frame's cached solutions into this frame's contacts, applying cached solutions as we go
  _ <- descZipVector fst fst useCache newCache 0 kContacts oldLagrangians
  frozenConstraints <- V.unsafeFreeze constraints
  return (lagrangians, frozenConstraints)
  where
    contactCount = length kContacts

-- | Solve the constraints for a given contact. (And apply the solution.)
improveContactSln ::
     (PrimMonad m)
  => SolutionProcessor (Double, Double) (ContactResult Lagrangian)
  -> ObjectFeatureKey Int
  -> Int
  -> V.MVector (PrimState m) (ObjectFeatureKey Int, ContactResult Lagrangian)
  -> V.Vector (ContactResult Constraint)
  -> World (PrimState m) label
  -> m ()
improveContactSln slnProc key@ObjectFeatureKey{..} i lagrangians constraints world = do
  (_, cached_l) <- MV.read lagrangians i
  let constraint = constraints V.! i
  phys_ab <- readPhysObjPair world _ofkObjKeys
  mu_ab <- readMuPair world _ofkObjKeys
  let new_l = lagrangian2 phys_ab <$> constraint
      Processed {..} = slnProc mu_ab cached_l new_l
  modifyPhysObjPair world (applySln _processedToApply constraint) _ofkObjKeys
  MV.write lagrangians i (key, _processedToCache)

-- | Run `improveSln` on every constraint in the world.
improveWorld ::
     (PrimMonad m)
  => SolutionProcessor (Double, Double) (ContactResult Lagrangian)
  -> Descending (ObjectFeatureKey Int, Flipping Contact')
  -> V.MVector (PrimState m) (ObjectFeatureKey Int, ContactResult Lagrangian)
  -> V.Vector (ContactResult Constraint)
  -> World (PrimState m) label
  -> m ()
improveWorld slnProc (Descending kContacts) lagrangians constraints world =
  mapM_ f $ zip [0..] kContacts
  where
    f (i, (key, _)) = improveContactSln slnProc key i lagrangians constraints world


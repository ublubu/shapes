{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}

module Physics.Solvers.Opt where

import Control.Lens
import Control.Monad
import Control.Monad.ST
import Data.Maybe
import qualified Data.HashTable.Class as H

import Physics.Constraint.Opt
import Physics.Constraints.Opt.Contact
import Physics.Contact.Opt
import Physics.Solvers.Opt.SolutionProcessors
import Physics.World.Opt
import Utils.Utils

prepareFrame :: (Contactable a)
             => [(Int, Int)]
             -> World a
             -> [(ObjectFeatureKey, Contact')]
prepareFrame pairKeys w =
  concatMap f pairKeys
  where f pairKey = keyedContacts pairKey (fromJust $ w ^? worldPair pairKey)

applyCachedSlns :: (Contactable a, H.HashTable h)
                => [ObjectFeatureKey]
                -> h s ObjectFeatureKey ContactSolution
                -> World a
                -> ST s (World a)
applyCachedSlns keys cache world =
  foldM f world keys
  where f world' key = applyCachedSln key cache world'

applyCachedSln :: (Contactable a, H.HashTable h)
               => ObjectFeatureKey
               -> h s ObjectFeatureKey ContactSolution
               -> World a
               -> ST s (World a)
applyCachedSln key@ObjectFeatureKey{..} cache world = do
  mSln <- H.lookup cache key
  return $ case mSln of
    Nothing -> world
    Just sln -> world & worldPair (fromSP _ofkObjKeys) %~ applySln sln

applySln :: (Contactable a)
         => ContactSolution
         -> (a, a)
         -> (a, a)
applySln ContactSolution{..} =
  applyConstraintResult _contactFriction . applyConstraintResult _contactNonPen

improveSln' :: (Contactable a, H.HashTable h)
            => SolutionProcessor a
            -> ObjectFeatureKey
            -> ContactSolution
            -> h s ObjectFeatureKey ContactSolution
            -> (a, a)
            -> ST s (a, a)
improveSln' slnProc key sln cache ab = do
  mCachedSln <- H.lookup cache key
  let (slnCache, slnApply) = case mCachedSln of
        Nothing -> (sln, sln)
        Just cachedSln -> slnProc cachedSln sln ab
  H.insert cache key slnCache
  return $ applySln slnApply ab

improveSln :: (Contactable a, H.HashTable h)
           => ContactBehavior
           -> SolutionProcessor a
           -> Double
           -> ObjectFeatureKey
           -> Contact'
           -> h s ObjectFeatureKey ContactSolution
           -> (a, a)
           -> ST s (a, a)
improveSln beh slnProc dt key contact cache ab =
  let sln = solveContact beh dt ab contact
  in improveSln' slnProc key sln cache ab

improveWorld' :: (Contactable a, H.HashTable h)
              => ContactBehavior
              -> SolutionProcessor a
              -> Double
              -> ObjectFeatureKey
              -> Contact'
              -> h s ObjectFeatureKey ContactSolution
              -> World a
              -> ST s (World a)
improveWorld' beh slnProc dt key@ObjectFeatureKey{..} contact cache =
  worldPair (fromSP _ofkObjKeys) f
  where f = improveSln beh slnProc dt key contact cache

improveWorld :: (Contactable a, H.HashTable h)
             => ContactBehavior
             -> SolutionProcessor a
             -> Double
             -> [(ObjectFeatureKey, Contact')]
             -> h s ObjectFeatureKey ContactSolution
             -> World a
             -> ST s (World a)
improveWorld beh slnProc dt kContacts cache world =
  foldM f world kContacts
  where f world' (key, contact) =
          improveWorld' beh slnProc dt key contact cache world'

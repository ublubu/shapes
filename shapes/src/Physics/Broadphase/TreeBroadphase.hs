{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Physics.Broadphase.TreeBroadphase where

import Control.Lens

import Data.Maybe
import qualified Data.Vector.Unboxed as V

import Physics.Broadphase.Aabb
import Physics.Broadphase.AabbTree
import qualified Physics.Constraint as C
import Physics.World.Class
import Physics.World.Object

import Utils.Descending

worldTree :: (PhysicsWorld Int w o, WorldObj ~ o) => w -> Maybe Node
worldTree w = treeFromList $ f <$> (itoListOf wObjs w)
  where f (key, obj) = (key, toAabb $ _worldShape obj)

-- | Find pairs of objects with overlapping AABBs.
-- Note: Pairs of static objects are excluded.
-- These pairs are in descending order according to 'unorderedPairs', where \"ascending\" is the world's traversal order.
culledKeys :: (V.Unbox k, PhysicsWorld k w o, WorldObj ~ o) => w -> Descending (k, k)
culledKeys w = Descending . catMaybes $ fmap f ijs
  where taggedAabbs = toTaggedAabbs isStatic w
        ijs = unorderedPairs $ V.length taggedAabbs
        -- NOTE: don't aabbCheck static objects, otherwise the sim explodes
        f (i, j) = if not (isStaticA && isStaticB) && aabbCheck a b then Just (i', j') else Nothing
          where (i', a, isStaticA) = taggedAabbs V.! i
                (j', b, isStaticB) = taggedAabbs V.! j
        {-# INLINE f #-}
        isStatic WorldObj{..} = C.isStatic $ C._physObjInvMass _worldPhysObj
        {-# INLINE isStatic #-}
{-# INLINE culledKeys #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Physics.Transform where

import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.List.Zipper
import Linear.Epsilon
import Linear.Matrix
import Linear.Metric
import Linear.V2
import Linear.Vector
import Utils.Utils
import Physics.Linear

type WorldTransform a = (V2 a -> V2 a, V2 a -> V2 a)

toTransform :: (Floating a) => V2 a -> a -> WorldTransform a
toTransform pos ori = (trans, untrans)
  where trans r = (rotate22 ori !* r) + pos
        untrans r = rotate22 (-ori) !* (r - pos)

idTransform :: WorldTransform a
idTransform = (id, id)

joinTransforms :: WorldTransform a -> WorldTransform a -> WorldTransform a
joinTransforms (outerF, outerG) (innerF, innerG) = (outerF . innerF, outerG . innerG)

invertTransform :: WorldTransform a -> WorldTransform a
invertTransform (f, g) = (g, f)

data WorldVelocity a = WorldVelocity { worldLinearVel :: V2 a
                                     , worldAngularVel :: a }

data WorldTransformable a => WorldT a = WorldT a (WorldTransform (WTNum a))
data WorldT' b a = WorldT' b (WorldTransform a)

wConvert' :: WorldTransformable a => WorldT a -> WorldT' a (WTNum a)
wConvert' (WorldT a b) = WorldT' a b

wConvert :: WorldTransformable a => WorldT' a (WTNum a) -> WorldT a
wConvert (WorldT' a b) = WorldT a b

-- wExtract and wInject don't change the transform - they only move between types
class WorldTransformable t where
  type WTNum t
  transform :: WorldTransform (WTNum t) -> t -> t
  untransform :: WorldTransform (WTNum t) -> t -> t
  wExtract :: WorldT t -> t
  wExtract (WorldT v t) = transform t v
  wInject :: WorldTransform (WTNum t) -> t -> WorldT t
  wInject t v = WorldT (untransform t v) t
  wInject_ :: WorldTransform (WTNum t) -> t -> t -- same as wInject, but throws away type information
  wInject_ = untransform

instance (Floating a) => WorldTransformable (V2 a) where
  type WTNum (V2 a) = a
  transform = fst
  untransform = snd

instance (WorldTransformable a) => WorldTransformable (WorldT a) where
  type WTNum (WorldT a) = WTNum a
  transform t' (WorldT v t) = WorldT v (joinTransforms t' t)
  untransform t' (WorldT v t) = WorldT v (joinTransforms (invertTransform t') t)
  wInject _ x = WorldT x idTransform

mapT' :: (WorldTransformable a) => (b -> a -> a) -> WorldT' b (WTNum a) -> a -> WorldT a
mapT' f (WorldT' s t) v = WorldT (f s v') t
  where v' = wInject_ t v

mapTT' :: (WorldTransformable a) => (b -> a -> a) -> WorldT' b (WTNum a) -> WorldT a -> WorldT a
mapTT' f s = mapT' f s . wExtract

mapT :: (WorldTransformable a) => (a -> b) -> WorldT a -> b
mapT f = f . wExtract

apT' :: (WorldTransformable a) => WorldT' (a -> b)

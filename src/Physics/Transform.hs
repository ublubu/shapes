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

data WorldTransformable b a => WorldT a b = WorldT (WorldTransform a) b
data WorldT' a b = WorldT' (WorldTransform a) b

instance Functor (WorldT' a) where
  fmap f (WorldT' t v) = WorldT' t (f v)

wConvert' :: WorldTransformable b a => WorldT a b -> WorldT' a b
wConvert' (WorldT a b) = WorldT' a b

wConvert :: WorldTransformable b a => WorldT' a b -> WorldT a b
wConvert (WorldT' a b) = WorldT a b

-- wExtract and wInject don't change the transform - they only move between types
class WorldTransformable t a where
  transform :: WorldTransform a -> t -> t
  untransform :: WorldTransform a -> t -> t

  wExtract :: WorldT a t -> t
  wExtract (WorldT t v) = transform t v

  wInject :: WorldTransform a -> t -> WorldT a t
  wInject t v = WorldT t (untransform t v)

  wInject_ :: WorldTransform a -> t -> t -- same as wInject, but throws away type information
  wInject_ = untransform

instance (Floating a) => WorldTransformable (V2 a) a where
  transform = fst
  untransform = snd

instance (WorldTransformable a b) => WorldTransformable (WorldT b a) b where
  transform t' (WorldT t v) = WorldT (joinTransforms t' t) v
  untransform t' (WorldT t v) = WorldT (joinTransforms (invertTransform t') t) v
  wInject _ = WorldT idTransform

wMap2' :: (WorldTransformable b n, WorldTransformable c n) => (a -> b -> c) -> WorldT' n a -> b -> WorldT n c
wMap2' f (WorldT' t s) v = WorldT t (f s v')
  where v' = wInject_ t v

wMap2'' :: (WorldTransformable b n, WorldTransformable c n) => (a -> b -> c) -> WorldT' n a -> WorldT n b -> WorldT n c
wMap2'' f s = wMap2' f s . wExtract

wExtractWith :: (WorldTransformable a n) => (a -> b) -> WorldT n a -> b
wExtractWith f = f . wExtract

wInjectWith'' :: (WorldTransformable a n) => WorldT' n (a -> b) -> WorldT n a -> WorldT' n b
wInjectWith'' f x = wInjectWith' f (wExtract x)

wInjectWith' :: (WorldTransformable a n) => WorldT' n (a -> b) -> a -> WorldT' n b
wInjectWith' (WorldT' t f) x = WorldT' t (f (wInject_ t x))

wApply' :: (WorldTransformable a n, WorldTransformable b n) => WorldT' n (a -> b) -> a -> b
wApply' f x = (wExtract . wConvert) (wInjectWith' f x)

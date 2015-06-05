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

data LocalT a b = LocalT (WorldTransform a) b

data WorldT a = WorldT a

iExtract :: WorldT a -> a
iExtract (WorldT x) = x

iInject :: a -> WorldT a
iInject = WorldT

instance Functor (LocalT a) where
  fmap f (LocalT t v) = LocalT t (f v)

instance Functor WorldT where
  fmap f (WorldT v) = WorldT (f v)

-- wExtract and wInject don't change the transform - they only move between types
class WorldTransformable t a where
  transform :: WorldTransform a -> t -> t
  untransform :: WorldTransform a -> t -> t

  wExtract :: LocalT a t -> WorldT t
  wExtract (LocalT t v) = WorldT (transform t v)

  wInject :: WorldTransform a -> WorldT t -> LocalT a t
  wInject t v = LocalT t (untransform t (iExtract v))

  wInject_ :: WorldTransform a -> t -> t -- same as wInject, but throws away type information
  wInject_ = untransform

instance (Floating a) => WorldTransformable (V2 a) a where
  transform = fst
  untransform = snd

instance (WorldTransformable a b) => WorldTransformable (LocalT b a) b where
  transform t' (LocalT t v) = LocalT (joinTransforms t' t) v
  untransform t' (LocalT t v) = LocalT (joinTransforms (invertTransform t') t) v
  wInject _ = LocalT idTransform . iExtract

iMap :: (a -> b) -> WorldT a -> WorldT b
iMap = fmap

iAp :: WorldT (a -> b) -> WorldT a -> WorldT b
iAp (WorldT f) = fmap f

iwAp :: (WorldTransformable a n) => WorldT (a -> b) -> LocalT n a -> WorldT b
iwAp f = iAp f . wExtract

wiAp :: (WorldTransformable a n) => LocalT n (a -> b) -> WorldT a -> LocalT n b
wiAp (LocalT t f) x = fmap f (wInject t x)

wAp :: (WorldTransformable a n) => LocalT n (a -> b) -> LocalT n a -> LocalT n b
wAp f x = wiAp f (wExtract x)

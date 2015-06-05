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

data WorldT a b = WorldT (WorldTransform a) b

data WorldI a = WorldI a

iExtract :: WorldI a -> a
iExtract (WorldI x) = x

iInject :: a -> WorldI a
iInject = WorldI

-- this is dangerous because a localized function could operate on a WorldT in another locale
instance Functor (WorldT a) where
  fmap f (WorldT t v) = WorldT t (f v)

instance Functor WorldI where
  fmap f (WorldI v) = WorldI (f v)

-- wExtract and wInject don't change the transform - they only move between types
class WorldTransformable t a where
  transform :: WorldTransform a -> t -> t
  untransform :: WorldTransform a -> t -> t

  wExtract :: WorldT a t -> WorldI t
  wExtract (WorldT t v) = WorldI (transform t v)

  wInject :: WorldTransform a -> WorldI t -> WorldT a t
  wInject t v = WorldT t (untransform t (iExtract v))

  wInject_ :: WorldTransform a -> t -> t -- same as wInject, but throws away type information
  wInject_ = untransform

instance (Floating a) => WorldTransformable (V2 a) a where
  transform = fst
  untransform = snd

instance (WorldTransformable a b) => WorldTransformable (WorldT b a) b where
  transform t' (WorldT t v) = WorldT (joinTransforms t' t) v
  untransform t' (WorldT t v) = WorldT (joinTransforms (invertTransform t') t) v
  wInject _ = WorldT idTransform . iExtract

iMap :: (a -> b) -> WorldI a -> WorldI b
iMap = fmap

iAp :: WorldI (a -> b) -> WorldI a -> WorldI b
iAp (WorldI f) = fmap f

iwAp :: (WorldTransformable a n) => WorldI (a -> b) -> WorldT n a -> WorldI b
iwAp f = iAp f . wExtract

wiAp :: (WorldTransformable a n) => WorldT n (a -> b) -> WorldI a -> WorldT n b
wiAp (WorldT t f) x = fmap f (wInject t x)

wAp :: (WorldTransformable a n) => WorldT n (a -> b) -> WorldT n a -> WorldT n b
wAp f x = wiAp f (wExtract x)

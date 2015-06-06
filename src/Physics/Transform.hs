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
import Linear.Affine
import Linear.Epsilon
import Linear.Matrix
import Linear.Metric
import Linear.V2
import Linear.Vector
import Utils.Utils
import Physics.Linear

type WorldTransform a = (M33 a, M33 a)

toTransform :: (Floating a) => Diff V2 a -> a -> WorldTransform a
toTransform pos ori = (transl !*! rot, rot' !*! transl')
  where rot = afrotate33 ori
        rot' = afrotate33 (-ori)
        transl = aftranslate33 pos
        transl' = aftranslate33 (-pos)

idTransform :: (Num a) => WorldTransform a
idTransform = (identity, identity)

joinTransforms :: (Num a) => WorldTransform a -> WorldTransform a -> WorldTransform a
joinTransforms (outer, outer') (inner, inner') = (outer !*! inner, inner' !*! outer')

invertTransform :: WorldTransform a -> WorldTransform a
invertTransform (f, g) = (g, f)

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

instance (Floating a) => WorldTransformable (P2 a) a where
  transform (trans, _) = afmul trans
  untransform (_, untrans) = afmul untrans

instance (Floating a) => WorldTransformable (V2 a) a where
  transform (trans, _) = afmul trans
  untransform (_, untrans) = afmul untrans

instance (Num b, WorldTransformable a b) => WorldTransformable (LocalT b a) b where
  transform t' (LocalT t v) = LocalT (joinTransforms t' t) v
  untransform t' (LocalT t v) = LocalT (joinTransforms (invertTransform t') t) v
  wInject _ = LocalT idTransform . iExtract

wfmap :: (Functor t) => (a -> t b) -> WorldT a -> t (WorldT b)
wfmap f (WorldT v) = fmap WorldT (f v)

wmap :: (a -> b) -> WorldT a -> WorldT b
wmap = fmap

wap :: WorldT (a -> b) -> WorldT a -> WorldT b
wap (WorldT f) = wmap f

wlap :: (WorldTransformable a n) => WorldT (a -> b) -> LocalT n a -> WorldT b
wlap f = wap f . wExtract

lwap :: (WorldTransformable a n) => LocalT n (a -> b) -> WorldT a -> LocalT n b
lwap (LocalT t f) x = lmap f (wInject t x)

lap :: (WorldTransformable a n) => LocalT n (a -> b) -> LocalT n a -> LocalT n b
lap f x = lwap f (wExtract x)

lmap :: (a -> b) -> LocalT n a -> LocalT n b
lmap = fmap

lfmap :: (Functor t) => (a -> t b) -> LocalT n a -> t (LocalT n b)
lfmap f (LocalT t v) = fmap (LocalT t) (f v)

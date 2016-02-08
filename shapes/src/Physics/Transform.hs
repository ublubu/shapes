{-# LANGUAGE DataKinds, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TypeFamilies, TypeSynonymInstances #-}

module Physics.Transform where

import Linear.Affine
import Linear.Matrix
import Linear.V2
import Utils.Utils
import Physics.Linear

type WorldTransform a = (M33 a, M33 a)

toTransform :: (Floating a) => Diff V2 a -> a -> WorldTransform a
toTransform pos ori = joinTransforms (translateTransform pos) (rotateTransform ori)

scaleTransform :: (Floating a) => V2 a -> WorldTransform a
scaleTransform s@(V2 x y) = (afscale33 s, afscale33 s')
  where s' = V2 (1/x) (1/y)

rotateTransform :: (Floating a) => a -> WorldTransform a
rotateTransform ori = (rot, rot')
  where rot = afrotate33 ori
        rot' = afrotate33 (-ori)

translateTransform :: (Floating a) => Diff V2 a -> WorldTransform a
translateTransform pos = (transl, transl')
  where transl = aftranslate33 pos
        transl' = aftranslate33 (-pos)

idTransform :: (Num a) => WorldTransform a
idTransform = (identity, identity)

joinTransforms :: (Num a) => WorldTransform a -> WorldTransform a -> WorldTransform a
joinTransforms (outer, outer') (inner, inner') = (outer !*! inner, inner' !*! outer')

joinTransforms' :: (Num a) => [WorldTransform a] -> WorldTransform a
joinTransforms' = foldl1 joinTransforms

invertTransform :: WorldTransform a -> WorldTransform a
invertTransform (f, g) = (g, f)

-- TODO: add another type variable to track values that originated in the same local space
-- see lap, Geometry.overlap
data LocalT a b = LocalT !(WorldTransform a) !b deriving (Show, Eq)
type LV2 a = LocalT a (V2 a)
type LP2 a = LocalT a (P2 a)

data WorldT a = WorldT !a deriving (Show, Eq)
type WV2 a = WorldT (V2 a)
type WP2 a = WorldT (Point V2 a)

iExtract :: WorldT a -> a
iExtract (WorldT x) = x

iInject :: a -> WorldT a
iInject = WorldT

iInject_ :: Num a => b -> LocalT a b
iInject_ = LocalT idTransform

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

  wExtract_ :: LocalT a t -> t
  wExtract_ = iExtract . wExtract

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

instance (Num a) => WorldTransformable (LocalT a b) a where
  transform t' (LocalT t v) = LocalT (joinTransforms t' t) v
  untransform t' (LocalT t v) = LocalT (joinTransforms (invertTransform t') t) v
  wInject _ = LocalT idTransform . iExtract

instance (WorldTransformable b a) => WorldTransformable (b, b) a where
  transform t = pairMap (transform t)
  untransform t = pairMap (untransform t)

instance (WorldTransformable b a) => WorldTransformable [b] a where
  transform t = map (transform t)
  untransform t = map (untransform t)

instance (WorldTransformable b a) => WorldTransformable (Maybe b) a where
  transform t = fmap (transform t)
  untransform t = fmap (untransform t)

wfmap :: (Functor t) => (a -> t b) -> WorldT a -> t (WorldT b)
wfmap f (WorldT v) = fmap WorldT (f v)

wflip :: (Functor t) => WorldT (t a) -> t (WorldT a)
wflip (WorldT v) = fmap WorldT v

wmap :: (a -> b) -> WorldT a -> WorldT b
wmap = fmap

wlift2 :: (a -> b -> c) -> WorldT a -> WorldT b -> WorldT c
wlift2 f x = wap (wmap f x)

wlift2_ :: (a -> b -> c) -> WorldT a -> WorldT b -> c
wlift2_ f x y = iExtract (wlift2 f x y)

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

lunsafe_ :: (a -> b) -> LocalT n a -> b
lunsafe_ f (LocalT _ v) = f v

wlens :: (Functor f) => (a -> f a) -> WorldT a -> f (WorldT a)
wlens f = fmap WorldT . f . iExtract

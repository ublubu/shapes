{-# Language MagicHash #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}

module Physics.Transform.Opt where

import GHC.Prim (Double#, (/##), negateDouble#)

import Utils.Utils
import Physics.Linear.Opt

type WorldTransform = SP M3x3 M3x3

toTransform :: V2 -> Double# -> WorldTransform
toTransform pos ori = joinTransforms (translateTransform pos) (rotateTransform ori)
{-# INLINE toTransform #-}

scaleTransform :: V2 -> WorldTransform
scaleTransform s@(V2 x y) = SP (afscale33 s) (afscale33 s')
  where s' = V2 (1.0## /## x) (1.0## /## y)
{-# INLINE scaleTransform #-}

rotateTransform :: Double# -> WorldTransform
rotateTransform ori = SP rot rot'
  where rot = afrotate33 ori
        rot' = afrotate33 (negateDouble# ori)
{-# INLINE rotateTransform #-}

translateTransform :: V2 -> WorldTransform
translateTransform pos = SP transl transl'
  where transl = aftranslate33 pos
        transl' = aftranslate33 (negateV2 pos)
{-# INLINE translateTransform #-}

idTransform :: WorldTransform
idTransform = SP identity3x3 identity3x3
{-# INLINE idTransform #-}

joinTransforms :: WorldTransform -> WorldTransform -> WorldTransform
joinTransforms (SP outer outer') (SP inner inner') = SP (outer `mul3x3x3` inner) (inner' `mul3x3x3` outer')
{-# INLINE joinTransforms #-}

joinTransforms' :: [WorldTransform] -> WorldTransform
joinTransforms' = foldl1 joinTransforms
{-# INLINE joinTransforms' #-}

invertTransform :: WorldTransform -> WorldTransform
invertTransform (SP f g) = SP g f
{-# INLINE invertTransform #-}

-- TODO: add another type variable to track values that originated in the same local space
-- see lap, Geometry.overlap
data LocalT b = LocalT !WorldTransform !b deriving Show
type LV2 = LocalT V2
type LP2 = LocalT P2

data WorldT a = WorldT !a deriving (Show, Eq)
type WV2 = WorldT V2
type WP2 = WorldT P2

iExtract :: WorldT a -> a
iExtract (WorldT x) = x
{-# INLINE iExtract #-}

iInject :: a -> WorldT a
iInject = WorldT
{-# INLINE iInject #-}

iInject_ :: b -> LocalT b
iInject_ = LocalT idTransform
{-# INLINE iInject_ #-}

instance Functor LocalT where
  fmap f (LocalT t v) = LocalT t (f v)
  {-# INLINE fmap #-}

instance Functor WorldT where
  fmap f (WorldT v) = WorldT (f v)
  {-# INLINE fmap #-}

-- wExtract and wInject don't change the transform - they only move between types
class WorldTransformable t where
  transform :: WorldTransform -> t -> t
  untransform :: WorldTransform -> t -> t

  wExtract :: LocalT t -> WorldT t
  wExtract (LocalT t v) = WorldT (transform t v)

  wExtract_ :: LocalT t -> t
  wExtract_ = iExtract . wExtract

  wInject :: WorldTransform -> WorldT t -> LocalT t
  wInject t v = LocalT t (untransform t (iExtract v))

  wInject_ :: WorldTransform -> t -> t -- same as wInject, but throws away type information
  wInject_ = untransform

instance WorldTransformable P2 where
  transform (SP trans _) = afmul' trans
  untransform (SP _ untrans) = afmul' untrans
  {-# INLINE transform #-}
  {-# INLINE untransform #-}

instance WorldTransformable V2 where
  transform (SP trans _) = afmul trans
  untransform (SP _ untrans) = afmul untrans
  {-# INLINE transform #-}
  {-# INLINE untransform #-}

instance (WorldTransformable t) => WorldTransformable (WorldT t) where
  transform t = WorldT . transform t . iExtract
  untransform t = WorldT . untransform t . iExtract
  {-# INLINE transform #-}
  {-# INLINE untransform #-}

instance WorldTransformable (LocalT b) where
  transform t' (LocalT t v) = LocalT (joinTransforms t' t) v
  untransform t' (LocalT t v) = LocalT (joinTransforms (invertTransform t') t) v
  wInject _ = LocalT idTransform . iExtract
  {-# INLINE transform #-}
  {-# INLINE untransform #-}
  {-# INLINE wInject #-}

instance (WorldTransformable b) => WorldTransformable (b, b) where
  transform t = pairMap (transform t)
  untransform t = pairMap (untransform t)
  {-# INLINE transform #-}
  {-# INLINE untransform #-}

instance (WorldTransformable b) => WorldTransformable [b] where
  transform t = map (transform t)
  untransform t = map (untransform t)
  {-# INLINE transform #-}
  {-# INLINE untransform #-}

instance (WorldTransformable b) => WorldTransformable (Maybe b) where
  transform t = fmap (transform t)
  untransform t = fmap (untransform t)
  {-# INLINE transform #-}
  {-# INLINE untransform #-}

data WaL w l = WaL { _wlW :: !(WorldT w)
                   , _wlL :: !(LocalT l)
                   } deriving (Show)
type WaL' t = WaL t t

instance (WorldTransformable w, WorldTransformable l) => WorldTransformable (WaL w l) where
  transform t (WaL w l) =
    WaL (transform t w) (transform t l)
  untransform t (WaL w l) =
    WaL (untransform t w) (untransform t l)
  {-# INLINE transform #-}
  {-# INLINE untransform #-}

wfmap :: (Functor t) => (a -> t b) -> WorldT a -> t (WorldT b)
wfmap f (WorldT v) = fmap WorldT (f v)
{-# INLINE wfmap #-}

wflip :: (Functor t) => WorldT (t a) -> t (WorldT a)
wflip (WorldT v) = fmap WorldT v
{-# INLINE wflip #-}

wmap :: (a -> b) -> WorldT a -> WorldT b
wmap = fmap
{-# INLINE wmap #-}

wlift2 :: (a -> b -> c) -> WorldT a -> WorldT b -> WorldT c
wlift2 f x = wap (wmap f x)
{-# INLINE wlift2 #-}

wlift2_ :: (a -> b -> c) -> WorldT a -> WorldT b -> c
wlift2_ f x y = iExtract (wlift2 f x y)
{-# INLINE wlift2_ #-}

wap :: WorldT (a -> b) -> WorldT a -> WorldT b
wap (WorldT f) = wmap f
{-# INLINE wap #-}

wlap :: (WorldTransformable a) => WorldT (a -> b) -> LocalT a -> WorldT b
wlap f = wap f . wExtract
{-# INLINE wlap #-}

lwap :: (WorldTransformable a) => LocalT (a -> b) -> WorldT a -> LocalT b
lwap (LocalT t f) x = lmap f (wInject t x)
{-# INLINE lwap #-}

lap :: (WorldTransformable a) => LocalT (a -> b) -> LocalT a -> LocalT b
lap f x = lwap f (wExtract x)
{-# INLINE lap #-}

lmap :: (a -> b) -> LocalT a -> LocalT b
lmap = fmap
{-# INLINE lmap #-}

lfmap :: (Functor t) => (a -> t b) -> LocalT a -> t (LocalT b)
lfmap f (LocalT t v) = fmap (LocalT t) (f v)
{-# INLINE lfmap #-}

lunsafe_ :: (a -> b) -> LocalT a -> b
lunsafe_ f (LocalT _ v) = f v
{-# INLINE lunsafe_ #-}

wlens :: (Functor f) => (a -> f a) -> WorldT a -> f (WorldT a)
wlens f = fmap WorldT . f . iExtract
{-# INLINE wlens #-}

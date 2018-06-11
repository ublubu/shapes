{-# Language MagicHash #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}

{- |
Types for keeping track of local spaces (transformed relative to global space).
Also, tools for creating and composing 2D transformations.
-}
module Physics.Transform where

import GHC.Prim (Double#, (/##), negateDouble#)

import Utils.Utils
import Physics.Linear

{- |
A pair of transformation matrices to and from world space, respectively.
See 'transform' and 'untransform'.

The transformation matrices are multiplied with (2D affine) column vectors.
-}
type WorldTransform = SP M3x3 M3x3

{- |
Create a 'WorldTransform' with a given translation and rotation.

Applying the resulting 'WorldTransform' in the forward direction
moves the origin to the first argument (:: 'V2'), and rotates
by the second argument (:: 'Double#').

Applying the result in the reverse direction will revert the transformation.
-}
toTransform :: V2 -- ^ Translation
            -> Double# -- ^ Rotation
            -> WorldTransform
toTransform pos ori = joinTransforms (translateTransform pos) (rotateTransform ori)

{- |
Create a 'WorldTransform' with a given scale.
-}
scaleTransform :: V2 -- ^ Scale
               -> WorldTransform
scaleTransform s@(V2 x y) = SP (afscale33 s) (afscale33 s')
  where s' = V2 (1.0## /## x) (1.0## /## y)

{- |
Create a 'WorldTransform' with a given rotation.
-}
rotateTransform :: Double# -- ^ Rotation
                -> WorldTransform
rotateTransform ori = SP rot rot'
  where rot = afrotate33 ori
        rot' = afrotate33 (negateDouble# ori)

-- | Create a 'WorldTransform' with a given translation.
translateTransform :: V2 -- ^ Translation
                   -> WorldTransform
translateTransform pos = SP transl transl'
  where transl = aftranslate33 pos
        transl' = aftranslate33 (negateV2 pos)

-- | Identity 'WorldTransform' does not alter the space.
idTransform :: WorldTransform
idTransform = SP identity3x3 identity3x3

-- | Sequence two 'WorldTransform's to produce a third.
joinTransforms :: WorldTransform -- ^ The outer transform - applied last
               -> WorldTransform -- ^ The inner transform - applied first
               -> WorldTransform -- ^ The composite transform
joinTransforms (SP outer outer') (SP inner inner') = SP (outer `mul3x3x3` inner) (inner' `mul3x3x3` outer')

-- | Sequence a list of 'WorldTransform's.
joinTransforms' :: [WorldTransform]
                -- ^ Transforms in order from outermost to innermost
                -> WorldTransform -- ^ The composite transform
joinTransforms' = foldl1 joinTransforms

-- | Reverse the direction of a 'WorldTransform'.
-- Simply swaps the two transformation matrices.
invertTransform :: WorldTransform -> WorldTransform
invertTransform (SP f g) = SP g f

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

iInject :: a -> WorldT a
iInject = WorldT

iInject_ :: b -> LocalT b
iInject_ = LocalT idTransform

instance Functor LocalT where
  fmap f (LocalT t v) = LocalT t (f v)

instance Functor WorldT where
  fmap f (WorldT v) = WorldT (f v)

-- wExtract and wInject don't change the transform - they only move between types
class WorldTransformable t where
  -- | Apply 'WorldTransform' in the forward direction (local space to world space).
  transform :: WorldTransform -> t -> t
  -- | Apply 'WorldTransform' in the reverse direction (world space to local space).
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

instance WorldTransformable V2 where
  transform (SP trans _) = afmul trans
  untransform (SP _ untrans) = afmul untrans

instance (WorldTransformable t) => WorldTransformable (WorldT t) where
  transform t = WorldT . transform t . iExtract
  untransform t = WorldT . untransform t . iExtract

instance WorldTransformable (LocalT b) where
  transform t' (LocalT t v) = LocalT (joinTransforms t' t) v
  untransform t' (LocalT t v) = LocalT (joinTransforms (invertTransform t') t) v
  wInject _ = LocalT idTransform . iExtract

instance (WorldTransformable b) => WorldTransformable (b, b) where
  transform t = pairMap (transform t)
  untransform t = pairMap (untransform t)

instance (WorldTransformable b) => WorldTransformable [b] where
  transform t = map (transform t)
  untransform t = map (untransform t)

instance (WorldTransformable b) => WorldTransformable (Maybe b) where
  transform t = fmap (transform t)
  untransform t = fmap (untransform t)

data WaL w l = WaL { _wlW :: !(WorldT w)
                   , _wlL :: !(LocalT l)
                   } deriving (Show)
type WaL' t = WaL t t

instance (WorldTransformable w) => WorldTransformable (WaL w l) where
  transform t (WaL w l) =
    WaL (transform t w) (transform t l)
  untransform t (WaL w l) =
    WaL (untransform t w) (untransform t l)

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

wlap :: (WorldTransformable a) => WorldT (a -> b) -> LocalT a -> WorldT b
wlap f = wap f . wExtract

lwap :: (WorldTransformable a) => LocalT (a -> b) -> WorldT a -> LocalT b
lwap (LocalT t f) x = lmap f (wInject t x)

lap :: (WorldTransformable a) => LocalT (a -> b) -> LocalT a -> LocalT b
lap f x = lwap f (wExtract x)

lmap :: (a -> b) -> LocalT a -> LocalT b
lmap = fmap

lfmap :: (Functor t) => (a -> t b) -> LocalT a -> t (LocalT b)
lfmap f (LocalT t v) = fmap (LocalT t) (f v)

lunsafe_ :: (a -> b) -> LocalT a -> b
lunsafe_ f (LocalT _ v) = f v

wlens :: (Functor f) => (a -> f a) -> WorldT a -> f (WorldT a)
wlens f = fmap WorldT . f . iExtract

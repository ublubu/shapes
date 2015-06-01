{-# LANGUAGE DataKinds #-}

module Physics.Geometry where

import Control.Monad
import Linear.Matrix
import Linear.Metric
import Linear.V2
import Utils.Utils
import Physics.Linear

furthestAlong :: (Metric t, Num a, Ord a) => t a -> [t a] -> t a
furthestAlong _ [] = error "furthestAlong isn't supposed to be used with []"
furthestAlong dir points = snd $ foldl1 g (fmap f points)
  where f point = (dir `dot` point, point)
        g a@(distA, _) b@(distB, _) = if distB > distA then b else a

data Shape a = Rectangle { rectangleWidth :: a
                         , rectangleHeight :: a }

data WorldTransform a = WorldTransform { worldPosition :: V2 a
                                       , worldOrientation :: a } |
                        WorldIdentity

data WorldVelocity a = WorldVelocity { worldLinearVel :: V2 a
                                     , worldAngularVel :: a}

data WorldT b a = WorldT (b a) (WorldTransform a)

vertices :: (Fractional a) => Shape a -> [V2 a]
vertices (Rectangle w h) = [V2 w2 h2, V2 w2 (-h2), V2 (-w2) (-h2), V2 (-w2) h2]
  where w2 = w / 2
        h2 = h / 2

transform :: (Floating a) => WorldTransform a -> V2 a -> V2 a
transform (WorldTransform pos ori) r = (rotate22 ori !* r) + pos
transform WorldIdentity r = r

untransform :: (Floating a) => WorldTransform a -> V2 a -> V2 a
untransform (WorldTransform pos ori) r = (rotate22 (-ori) !* r) - pos
untransform WorldIdentity r = r

support_ :: (Floating a, Ord a) => Shape a -> V2 a -> V2 a
support_ shape dir = furthestAlong dir (vertices shape)

support :: (Floating a, Ord a) => WorldT Shape a -> V2 a -> V2 a
support (WorldT shape trans) = untransform trans . support_ shape . transform trans

flipSupport :: (Floating a, Ord a) => (V2 a -> V2 a) -> V2 a -> V2 a
flipSupport f dir = (-(f (-dir)))

minkowskiSupport :: (Floating a, Ord a) => WorldT Shape a -> WorldT Shape a -> V2 a -> V2 a
minkowskiSupport a b dir = support a dir + flipSupport (support b) dir

data Simplex a = Simplex1 (V2 a) | Simplex2 (V2 a) (V2 a) | Simplex3 (V2 a) (V2 a) (V2 a)

containOrigin :: (Num a, Ord a) => (V2 a -> V2 a) -> Simplex a -> Maybe (Simplex a)
containOrigin f (Simplex1 a) = toMaybe passedOrigin =<< containOrigin f (Simplex2 a' a)
  where dir = (-a)
        a' = f dir
        passedOrigin = not (similarDir a' a)
containOrigin f (Simplex2 a b) = if similarDir ab a
                                 then containOrigin f (Simplex1 a)
                                 else containOrigin f (Simplex3 (f dir) a b)
  where ab = b - a
        a0 = (-a)
        dir = ab `perpendicularTowards` a0
containOrigin f s@(Simplex3 a b c) =
  if similarDir acOut a0
  then if similarDir ac a0 then containOrigin f (Simplex2 a c)
       else star
  else if similarDir abOut a0 then star
       else Just s
  where acOut = ac `perpendicularTowards` ba
        ac = c - a
        ba = a - b
        abOut = ab `perpendicularTowards` ca
        ab = (-ba)
        ca = (-ac)
        a0 = (-a)
        star = if similarDir ab a0 then containOrigin f (Simplex2 a b)
               else containOrigin f (Simplex1 a)

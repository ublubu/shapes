{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Physics.Linear where

import GHC.TypeLits (Nat, type(+))
import Control.Lens
import qualified Data.Vector as Vec
import Data.Maybe
import Linear.Affine
import Linear.V
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Matrix
import Linear.Metric

type V6 a = V 6 a
type M66 a = V6 (V6 a)
type P2 a = Point V2 a

join22 :: V2 a -> V2 a -> V4 a
join22 (V2 ax ay) (V2 bx by) = V4 ax ay bx by

join33 :: V3 a -> V3 a -> V 6 a
join33 (V3 a b c) (V3 d e f) = listToV [a, b, c, d, e, f]

join44 :: V4 a -> V4 a -> V 8 a
join44 (V4 a b c d) (V4 e f g h) = listToV [a, b, c, d, e, f, g, h]

cross22 :: Num a => V2 a -> V2 a -> a
cross22 (V2 ax ay) (V2 bx by) = (ax * by) - (ay * bx)

append2 :: V2 a -> a -> V3 a
append2 (V2 a b) = V3 a b

listToV :: Dim n => [a] -> V (n :: Nat) a
listToV = fromJust . fromVector . Vec.fromList

rotate22 :: (Floating a) => a -> M22 a
rotate22 ori = V2 (V2 c (-s)) (V2 s c)
  where c = cos ori
        s = sin ori

afmat33 :: (Num a) => M22 a -> M33 a
afmat33 (V2 x y) = (V3 (append2 x 0) (append2 y 0) (V3 0 0 1))

aftranslate33 :: (Num a) => V2 a -> M33 a
aftranslate33 (V2 x y) = V3 (V3 1 0 x) (V3 0 1 y) (V3 0 0 1)

afrotate33 :: (Floating a) => a -> M33 a
afrotate33 = afmat33 . rotate22

dfa :: (Num a) => Diff V2 a -> V3 a
dfa (V2 x y) = V3 x y 0

afd :: V3 a -> Diff V2 a
afd = view _xy

pfa :: (Num a) => P2 a -> V3 a
pfa (P (V2 x y)) = V3 x y 1

afp :: V3 a -> P2 a
afp = P . view _xy

afdot :: (Num a) => P2 a -> Diff V2 a -> a
afdot a b = view _Point a `dot` b

afdot' :: (Num a) => Diff V2 a -> P2 a -> a
afdot' = flip afdot


class AffineTrans t a where
  afmul :: M33 a -> t -> t

instance (Num a) => AffineTrans (Point V2 a) a where
  afmul m = afp . (m !*) . pfa

instance (Num a) => AffineTrans (V2 a) a where
  afmul m = afd . (m !*) . dfa

clockwise22 :: Num a => M22 a
clockwise22 = V2 (V2 0 1) (V2 (-1) 0)

clockwise2 :: Num a => V2 a -> V2 a
clockwise2 (V2 x y) = V2 y (-x)

anticlockwise22 :: Num a => M22 a
anticlockwise22 = V2 (V2 0 1) (V2 (-1) 0)

anticlockwise2 :: Num a => V2 a -> V2 a
anticlockwise2 (V2 x y) = V2 (-y) x

perpendicularTowards :: (Num a, Ord a) => V2 a -> V2 a -> V2 a
a `perpendicularTowards` b = rot b
  where rot = if a `cross22` b > 0 then clockwise2
              else anticlockwise2

similarDir :: (Metric t, Num a, Ord a) => t a -> t a -> Bool
similarDir a b = a `dot` b > 0


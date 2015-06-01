{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}

module Physics.Linear where

import GHC.TypeLits (Nat, type(+))
import qualified Data.Vector as Vec
import Data.Maybe
import Linear.V
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Matrix
import Linear.Metric

type V6 a = V 6 a
type M66 a = V6 (V6 a)

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


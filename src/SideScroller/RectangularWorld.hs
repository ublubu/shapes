module SideScroller.RectangularWorld where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Monoid
import Directional
import Geometry
import Utils.Utils
import Debug.Trace
import SideScroller.Calculus

data Box a = Box (Transform (Pos a)) (Speed a)

data Boxes a b = Boxes { boxes :: [Box b]
                       , boxesTime :: a }

hasCollision :: (Ord a) => Rectangular a -> Rectangular a -> Bool
hasCollision rectA rectB = getAny . fold . fmap Any $ (overlap <$> boundsA <*> boundsB)
  where boundsA = rectToAxisAligned rectA
        boundsB = rectToAxisAligned rectB

hasCollision_ :: (Ord a) => Rectangular a -> Rectangular a -> Bool
hasCollision_ rectA rectB = getAny . fold . fmap Any $ (between <$> bounds <*> rectB)
  where bounds = degenerateRect_ (rectToAxisAligned rectA)

overlap :: (Ord a) => Pair a -> Pair a -> Bool
overlap a b = getAny . fold $ (f <$> Pair a b <*> Pair b a)
  where f aa bb = fold . fmap Any $ fmap (between aa) bb

between :: (Ord a) => Pair a -> a -> Bool
between (Pair a b) x = if a < b then a <= x && x <= b
                   else b <= x && x <= a

factorFromBaseline :: (Fractional a) => a -> a -> a -> a
factorFromBaseline baseline a b = (b - baseline) / (a - baseline)

factorBetween :: (Fractional a, Ord a) => Pair a -> a -> Maybe a
factorBetween (Pair a b) x = toMaybe (factor >= 0 && factor <= 1) factor
  where factor = factorFromBaseline a b x

-- rectA is moving by dx
checkCollision :: (Fractional a, Ord a) => Rectangular a -> Rectangular a -> Pair a -> Maybe (GridOriented a)
checkCollision rectA rectB dx = collapse (>) collisions
  where rectX = degenerateRect dx
        boundsA = (\a x -> Pair a (a + x)) <$> rectA <*> rectX
        rectB' = reverseRect rectB
        potentials = factorBetween <$> boundsA <*> rectB'
        toBounds rect = degenerateRect_ $ (swapAxes . rectToAxisAligned) rect
        toCollision p a b = toCollision_ a b =<< p
        toCollision_ a b p = toMaybe (overlap a' b) p
          where a' = a + (fmap (*p) dx)
        collisions = toCollision <$> potentials <*> toBounds rectA <*> toBounds rectB

advanceStep :: (Convert a b, Num a, Num b) => Boxes a b -> a -> Boxes a b
advanceStep (Boxes bs t) dt = Boxes (fmap f bs) (t + dt)
  where f (Box (Transform scale origin) speed) = Box (Transform scale (origin `aplus` dx)) speed
          where dx = speed `integral` (convert dt)

advanceTime :: (Convert a b, Num b, RealFrac a) => Boxes a b -> a -> a -> (Boxes a b, Boxes a b)
advanceTime bs dt tstep = (bs', bs'')
  where steps = floor (dt / tstep)
        remaining = max 0 (dt - ((fromIntegral steps) * tstep))
        bs' = iterate (`advanceStep` tstep) bs !! fromIntegral steps
        bs'' = bs' `advanceStep` remaining

module SmoothSlidingGrid where

import Data.Foldable
import Data.Monoid
import Foreign.C.Types
import Grid
import SlidingGrid
import FreezableT
import SDL.Geometry
import Debug.Trace

data SmoothSliding a = SmoothSliding (GeomPoint) a deriving Show

changeSlideAmount :: (GeomPoint -> GeomPoint) -> SmoothSliding a -> SmoothSliding a
changeSlideAmount f (SmoothSliding amount x) = SmoothSliding (f amount) x

setSlideAmount :: GeomPoint -> SmoothSliding a -> SmoothSliding a
setSlideAmount x = changeSlideAmount (\_ -> x)

instance Functor SmoothSliding where
  fmap f (SmoothSliding x y) = SmoothSliding x (f y)

toPartialSlide :: GeomPoint -> (SlideDirection (SmoothSliding a), GeomPoint)
toPartialSlide (x, y)
  | abs x > abs y = if x > 0 then (rightSliding, (x, 0))
                    else (leftSliding, (x, 0))
  | otherwise = if y > 0 then (downSliding, (0, y))
                else (upSliding, (0, y))

partialSlide :: Show a => GeomPoint -> TileZipper (SmoothSliding a) -> Maybe (TileZipper (SmoothSliding a))
partialSlide x = trace "in partialSlide" $ slideMap dir (\z' -> fmap (setSlideAmount x') (item z'))
  where (dir, x') = toPartialSlide x


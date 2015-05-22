module TileRider.SmoothSlidingGrid where

import Control.Applicative
import Data.Foldable
import Data.Monoid
import Data.Maybe
import Foreign.C.Types
import Directional hiding (toDirection)
import TileRider.Grid
import qualified TileRider.Grid as Grid
import TileRider.SlidingGrid
import FreezableT
import SDL.Geometry
import Utils.Utils

data SmoothSliding a = SmoothSliding (GeomPoint) a deriving Show

toSmoothSliding :: TileZipper a -> TileZipper (SmoothSliding a)
toSmoothSliding = tilesMap (SmoothSliding zero)

changeSlideAmount :: (GeomPoint -> GeomPoint) -> SmoothSliding a -> SmoothSliding a
changeSlideAmount f (SmoothSliding amount x) = SmoothSliding (f amount) x

setSlideAmount :: GeomPoint -> SmoothSliding a -> SmoothSliding a
setSlideAmount x = changeSlideAmount (\_ -> x)

instance Functor SmoothSliding where
  fmap f (SmoothSliding x y) = SmoothSliding x (f y)

toPartialSlide :: (Num a, Ord a) => Point a -> (GridDirection, Point a)
toPartialSlide (x, y)
  | abs x > abs y = if x > 0 then (GridRight, (x, 0))
                    else (GridLeft, (x, 0))
  | y > 0 = (GridDown, (0, y))
  | otherwise = (GridUp, (0, y))

partialSlide :: GridDirection -> GeomPoint -> TileZipper (SmoothSliding a) -> Maybe (TileZipper (SmoothSliding a))
partialSlide dir amount = slideMap dir f
  where f z' = fmap (setSlideAmount amount) item
          where item = gridItem z'

toTileCoord :: RealFrac a => Point a -> Point a -> Point a -> Point Int
toTileCoord scale origin x = pairMap floor (pairAp (pairMap (/) (x - origin)) scale)

clickTile :: RealFrac a  =>  Point a -> Point a -> Point a -> TileZipper b -> Maybe (TileZipper b)
clickTile scale origin click = Grid.moveTo (toTileCoord scale origin click)

dragDistance :: Num a => Point (Point a) -> Point a
dragDistance (x, x') = x' - x


module SmoothSlidingGrid where

import Control.Applicative
import Data.Foldable
import Data.Monoid
import Data.Maybe
import Debug.Trace
import Foreign.C.Types
import Directional hiding (toDirection)
import Grid
import SlidingGrid
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
  | abs x > abs y = if x > 0 then trace "RIGHT" (GridRight, (x, 0))
                    else trace "LEFT" (GridLeft, (x, 0))
  | y > 0 = trace "DOWN" (GridDown, (0, y))
  | otherwise = trace "UP" (GridUp, (0, y))

partialSlide :: GeomPoint -> TileZipper (SmoothSliding a) -> Maybe (TileZipper (SmoothSliding a))
partialSlide drag = slideMap dir f
  where (dir, amount) = toPartialSlide drag
        f z' = fmap (setSlideAmount amount) item
          where item = gridItem z'

applyPartialSlide :: (RealFrac a) => Point a -> Point a -> Point (Point a) -> TileZipper b -> TileZipper (SmoothSliding b)
applyPartialSlide scale origin drag@(x, _) z =
  fromMaybe sz $ partialSlide distance =<< sz'
  where distance = toGeomPoint (dragDistance drag)
        sz = toSmoothSliding z
        sz' = clickTile scale origin x sz

tileOrigin :: Num a => Point a -> Point a -> TileZipper b -> Point a
tileOrigin scale origin z = origin + (scale * coord)
  where coord = pairMap fromIntegral $ gridCoord z

toTileCoordInt :: Integral a => Point a -> Point a -> Point a -> Point Int
toTileCoordInt scale origin x = pairMap fromIntegral (pairAp (pairMap quot (x - origin)) scale)

toTileCoord :: RealFrac a => Point a -> Point a -> Point a -> Point Int
toTileCoord scale origin x = pairMap floor (pairAp (pairMap (/) (x - origin)) scale)

clickTile :: RealFrac a  =>  Point a -> Point a -> Point a -> TileZipper b -> Maybe (TileZipper b)
clickTile scale origin click = Grid.moveTo (toTileCoord scale origin click)

dragDistance :: Num a => Point (Point a) -> Point a
dragDistance (x, x') = x' - x

toDirection :: (Num a, Ord a) => Point (Point a) -> GridDirection
toDirection d
  | abs x > abs y = if x > 0 then GridRight
                    else GridLeft
  | y > 0 = GridDown
  | otherwise = GridUp
  where (x, y) = dragDistance d

toBoundingRect :: Num a => Point a -> Point a -> Point a -> GridDirection -> Rectangular a
toBoundingRect scale origin click dir = extend <*> rect' <*> scaledSignedRect scale
  where scale' = scale + (1, 1)
        origin' = origin - (1, 1)
        click' = (+) <$> signedRect <*> degenerateRect click
        rect = (+) <$> fromBottomRight scale' <*> degenerateRect origin'
        rect' = clip <*> click' <*> rect
        clip = injectOriented_ (GridOriented dir const) (\_ x -> x)
        extend = injectOriented_ (GridOriented dir (+)) const

applyDrag :: (Ord a, RealFrac a) => Point a -> Point a -> Point (Point a) -> TileZipper b -> (Point (Point a), Maybe (TileZipper b))
applyDrag scale origin drag@(x, x'') z = case intersection of
  Nothing -> (drag, Nothing)
  Just (tile, (GridOriented slideDir x')) ->
    if slideDir == dir
    then ((x', x''), slide_ slideDir tile)
    else ((x', x''), Nothing)
  where intersection = do
          tile <- clickTile scale origin x z
          x' <- let origin' = tileOrigin scale origin tile
                    bounds = toBoundingRect scale origin' x dir in
            drag `intersect` bounds
          return (tile, x')
        dir = toDirection drag

completelyApplyDrag :: (Ord a, RealFrac a) => Point a -> Point a -> Point (Point a) -> TileZipper b -> (Point (Point a), Maybe (TileZipper b))
completelyApplyDrag scale origin d z = case z' of
  Nothing -> (d', Nothing)
  Just zz' -> case z'' of
    Nothing -> (d'', z')
    Just _ -> (d'', z'')
    where (d'', z'') = completelyApplyDrag scale origin d' zz'
  where (d', z') = applyDrag scale origin d z

slideTiles :: (Ord a, RealFrac a) => Point a -> Point a -> Point (Point a) -> TileZipper b -> (Point (Point a), TileZipper b)
slideTiles scale origin drag z = (d', fromMaybe z z')
  where (d', z') = completelyApplyDrag scale origin drag z

-- This is how the sliding works
-- * The initial click selects the tile
-- * The end position of the click+drag selects the direction
--     Use this direction to decide when the path falls off the sliding tile
--       (include the space the tile could slide into)
-- * If the path falls off the tile, split that part off as a new click+drag
--     Did the path fall off the tile before it finished moving one tile length?
--       yes -> put the tile back
--       no -> move the tile
-- * If the path never comes off the tile, leave this click+drag for the next round
--     Calculate a partial move for the rendering
--
-- Drag after click+drag? merge the drag with whatever click+drag and do the above ^



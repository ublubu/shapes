module SmoothSlidingGrid where

import Control.Applicative
import Data.Foldable
import Data.Monoid
import Data.Maybe
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

toTileCoordInt :: Integral a => Point a -> Point a -> Point a -> Point Int
toTileCoordInt scale origin x = pairMap fromIntegral (pairAp (pairMap quot (x - origin)) scale)

toTileCoord :: RealFrac a => Point a -> Point a -> Point a -> Point Int
toTileCoord scale origin x = pairMap floor (pairAp (pairMap (/) (x - origin)) scale)

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
toBoundingRect scale origin click dir = extend <*> rect' <*> degenerateRect scale
  where scale' = scale + (1, 1)
        origin' = origin - (1, 1)
        click' = (+) <$> signedRect <*> degenerateRect click
        rect = (+) <$> fromBottomRight scale' <*> degenerateRect origin'
        rect' = clip <*> click' <*> rect
        clip = injectOriented_ (GridOriented dir const) (\_ x -> x)
        extend = injectOriented_ (GridOriented dir (+)) const

applyDrag :: (Ord a, Fractional a) => Point a -> Point a -> Point (Point a) -> TileZipper b -> (Point (Point a), Maybe (TileZipper b))
applyDrag scale origin drag@(x, x'') z = case mx' of
  Nothing -> (drag, Nothing)
  Just (GridOriented slideDir x') -> if slideDir == dir
                                      then ((x', x''), slide_ slideDir z)
                                      else ((x', x''), Nothing)
  where mx' = intersect drag bounds
        dir = toDirection drag
        bounds = toBoundingRect scale origin x dir

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



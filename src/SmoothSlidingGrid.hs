module SmoothSlidingGrid where

import Data.Foldable
import Data.Monoid
import Data.Maybe
import Foreign.C.Types
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

toTileCoord :: GeomPoint -> GeomPoint -> GeomPoint -> Point Int
toTileCoord scale origin x = pairMap fromIntegral (pairAp (pairMap quot (x - origin)) scale)

data Drag = Drag GeomPoint GeomPoint

advanceDrag :: GeomPoint -> Drag -> Drag
advanceDrag x (Drag y y') = Drag (y + x) y'

dragDistance :: Drag -> GeomPoint
dragDistance (Drag x x') = x' - x

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


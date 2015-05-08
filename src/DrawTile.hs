module DrawTile where

import qualified Graphics.UI.SDL.Types as SDL.T
import Data.Maybe
import SDL.Geometry
import SDL.Draw
import SlidingGrid
import SmoothSlidingGrid
import GameInput
import Grid
import Utils.Utils

data GridDrawInfo = GridDrawInfo { tileSize :: GeomPoint
                                   , gridOrigin :: GeomPoint } deriving Show

fromTileSize :: Num a => GridDrawInfo -> Point a
fromTileSize i = pairMap fromIntegral (tileSize i)

fromGridOrigin :: Num a => GridDrawInfo -> Point a
fromGridOrigin i = pairMap fromIntegral (gridOrigin i)

drawTileAt :: SDL.T.Renderer -> SDL.T.Rect -> Tile a -> IO ()
drawTileAt r target t = do
  setColor r color
  fillRectangle r target
    where color = case t of
            EmptyTile -> Yellow
            FixedTile _ -> Green
            SlidingTile _ -> Blue

drawTile :: SDL.T.Renderer -> GridDrawInfo -> TileZipper (SmoothSliding a) -> IO ()
drawTile r (GridDrawInfo scale@(w, h) origin) t = case tile of
  EmptyTile -> return ()
  SlidingTile x -> f x
  FixedTile x -> f x
  where tile = gridItem t
        coord = pairMap fromIntegral $ gridCoord t
        f (SmoothSliding offset _) = drawTileAt r target tile
          where (x, y) = (scale * coord) + origin + offset
                target = toRect x y w h

drawTiles :: RealFrac b => SDL.T.Renderer -> GridDrawInfo -> GridInput -> TileZipper a -> IO ()
drawTiles r drawInfo (GridInput drag) z = gridSequenceFrom (drawTile r drawInfo) z'
  where z' = fromMaybe (error "wtf, there's no (0, 0) tile?") (Grid.moveTo zero g)
        g = case drag of
          Nothing -> toSmoothSliding z
          Just drag' -> applyPartialSlide scale origin drag' z
        scale = fromTileSize drawInfo
        origin = fromGridOrigin drawInfo




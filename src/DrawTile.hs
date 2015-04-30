module DrawTile where

import qualified Graphics.UI.SDL.Types as SDL.T
import SDL.Geometry
import SDL.Draw
import SlidingGrid
import SmoothSlidingGrid
import Grid
import Utils.Utils

drawTileAt :: SDL.T.Renderer -> SDL.T.Rect -> Tile a -> IO ()
drawTileAt r target t = do
  setColor r color
  fillRectangle r target
    where color = case t of
            EmptyTile -> Yellow
            FixedTile _ -> Green
            SlidingTile _ -> Blue

drawTile :: SDL.T.Renderer -> GeomPoint -> GeomPoint -> TileZipper (SmoothSliding a) -> IO ()
drawTile r scale@(w, h) origin t = case tile of
  EmptyTile -> return ()
  SlidingTile x -> f x
  FixedTile x -> f x
  where tile = gridItem t
        coord = pairMap fromIntegral $ gridCoord t
        f (SmoothSliding offset _) = drawTileAt r target tile
          where (x, y) = (scale * coord) + origin + offset
                target = toRect x y w h

drawTiles :: SDL.T.Renderer -> GeomPoint -> GeomPoint -> TileZipper (SmoothSliding a) -> IO ()
drawTiles r scale origin = gridSequenceFrom (drawTile r scale origin)


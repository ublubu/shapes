module DrawTile where

import qualified Graphics.UI.SDL.Types as SDL.T
import SDL.Geometry
import SDL.Draw
import SlidingGrid
import Grid
import Utils.Utils

drawTileAt :: SDL.T.Renderer -> SDL.T.Rect -> Tile a -> IO ()
drawTileAt r target t = do
  setColor r color
  fillRectangle r target
    where color = case t of
            EmptyTile -> White
            FixedTile _ -> Green
            SlidingTile _ -> Blue

drawTile :: SDL.T.Renderer -> GeomPoint -> GeomPoint -> TileZipper a -> IO ()
drawTile r scale@(w, h) origin t = drawTileAt r target (item t)
  where coord = pairMap fromIntegral $ gridCoord t
        (x, y) = (scale * coord) + origin
        target = toRect x y w h

drawTiles :: SDL.T.Renderer -> GeomPoint -> GeomPoint -> TileZipper a -> IO ()
drawTiles r scale origin = gridSequenceFrom (drawTile r scale origin)


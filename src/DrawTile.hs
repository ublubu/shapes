module DrawTile where

import qualified Graphics.UI.SDL.Types as SDL.T
import Control.Monad
import Control.Applicative
import Data.Maybe
import Foreign.C.Types
import SDL.Geometry
import SDL.Draw
import Directional
import SlidingGrid
import SmoothSlidingGrid
import GameInput
import GameState
import GameTile
import Grid
import Utils.Utils

drawTileAt :: SDL.T.Renderer -> GeomPoint -> GeomPoint -> GameTile -> IO ()
drawTileAt r scale@(w, h) origin@(x, y) (GameTile path role) = case role of
  PathTile -> f Green
  GoalTile -> f Blue
  SpawnTile -> f Red
  NothingTile -> f Yellow
  where f color = do
          setColor r color
          fillRectangle r target
          setColor r Black
          sequenceRect (drawTilePaths r scale origin <*> path)
        target = SDL.Geometry.toRect x y w h

drawTilePaths :: SDL.T.Renderer -> GeomPoint -> GeomPoint -> Rectangular (Bool -> IO ())
drawTilePaths r scale origin = f <$> center <*> outer
  where scale' = pairMap (`quot` 2) scale
        center = pure (origin + scale')
        outer = (+) <$> center <*> ((* scale') <$> unitGeomPoint)
        f x x' b = when b $ drawLine r x x'

drawTile :: SDL.T.Renderer -> GridDrawInfo CInt -> TileZipper (SmoothSliding GameTile) -> IO ()
drawTile r (GridDrawInfo scale@(w, h) origin) t = case tile of
  EmptyTile -> return ()
  SlidingTile x -> f x
  FixedTile x -> f x
  where tile = gridItem t
        coord = pairMap fromIntegral $ gridCoord t
        f (SmoothSliding offset gameTile) = drawTileAt r scale origin' gameTile
          where origin' = (scale * coord) + origin + offset

drawTiles :: SDL.T.Renderer -> GridDrawInfo CInt -> GridInput -> TileZipper GameTile -> IO ()
drawTiles r drawInfo (GridInput drag) z = gridSequenceFrom (drawTile r drawInfo) z'
  where z' = fromMaybe (error "wtf, there's no (0, 0) tile?") (Grid.moveTo zero g)
        g = case drag of
          Nothing -> toSmoothSliding z
          Just drag' -> applyPartialSlide scale origin drag' z
        scale = fromTileSize fromIntegral drawInfo
        origin = fromGridOrigin fromIntegral drawInfo

drawGrid :: SDL.T.Renderer -> GridDrawInfo CInt -> GridInput -> GridState -> IO ()
drawGrid r drawInfo gi@(GridInput drag) (GridState _ _ tiles) =
  drawTiles r drawInfo gi tiles


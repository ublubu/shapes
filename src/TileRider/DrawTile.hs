module TileRider.DrawTile where

import qualified Graphics.UI.SDL.Types as SDL.T
import Control.Monad
import Control.Applicative
import Data.Maybe
import Foreign.C.Types
import SDL.Geometry
import SDL.Draw
import Directional
import TileRider.Drag
import TileRider.SlidingGrid
import TileRider.SmoothSlidingGrid
import TileRider.GameInput
import TileRider.GameState
import TileRider.GameTile
import TileRider.Grid
import qualified TileRider.Grid as Grid
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
drawTile r (GridDrawInfo scale origin) t = case tile of
  EmptyTile -> return ()
  SlidingTile x -> f x
  FixedTile x -> f x
  where tile = gridItem t
        coord = pairMap fromIntegral $ gridCoord t
        f (SmoothSliding offset gameTile) = drawTileAt r scale origin' gameTile
          where origin' = (scale * coord) + origin + offset

drawTiles :: RealFrac a => SDL.T.Renderer -> GridDrawInfo CInt -> Maybe (PartialMoveResult a) -> TileZipper GameTile -> IO ()
drawTiles r drawInfo moveM z = gridSequenceFrom (drawTile r drawInfo) szz''
  where szz'' = fromMaybe (error "wtf, there's no (0, 0) tile?") (Grid.moveTo zero sz')
        sz' = case moveM of
          Nothing -> sz
          Just move@(PartialMoveResult dir _ coord) ->
            fromMaybe (error "the partial move failed!") szM'
            where szM' = atCoord (partialSlide dir (toGeomPoint $ offsetAmount_ move)) coord sz
        sz = toSmoothSliding z

drawPlayer :: RealFrac a => SDL.T.Renderer -> GridDrawInfo CInt -> Maybe (PartialMoveResult a) -> Point Int -> IO ()
drawPlayer r (GridDrawInfo scale origin) move p = do
  setColor r Black
  fillRectangle r target
  where (x0, y0) = origin + (toGeomPointInt p * scale) + pairMap (`quot` 4) scale + offset
        (w', h') = pairMap (`quot` 2) scale
        target = SDL.Geometry.toRect x0 y0 w' h'
        offset = toGeomPoint (offsetAmount move)

drawGrid :: SDL.T.Renderer -> GridDrawInfo CInt -> GridInput -> GridState -> IO ()
drawGrid r drawInfo (GridInput _ partialMove) (GridState player _ tiles) = do
  drawTiles r drawInfo tileMove tiles
  drawPlayer r drawInfo playerMove player
  where (tileMove, playerMove) = case partialMove of
          Nothing -> (Nothing, Nothing)
          Just (PartialMoveResult _ _ coord) -> if coord == player
                                                then (Nothing, partialMove)
                                                else (partialMove, Nothing)

offsetAmount :: Num a => Maybe (PartialMoveResult a) -> Point a
offsetAmount = maybe zero offsetAmount_

offsetAmount_ :: Num a => PartialMoveResult a -> Point a
offsetAmount_ (PartialMoveResult dir dist _) = toPoint $ injectAxis (toAxis $ GridOriented dir dist) 0


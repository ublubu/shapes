module Game where

import qualified Graphics.UI.SDL.Video as SDL.V
import qualified Graphics.UI.SDL.Enum as SDL.E
import qualified Graphics.UI.SDL.Types as SDL.T
import qualified Graphics.UI.SDL.Basic as SDL.B
import qualified Graphics.UI.SDL.Timer as SDL.Timer
import qualified Graphics.UI.SDL.Event as SDL.Event
import Control.Monad
import Foreign.C.Types
import SDL.Draw
import SDL.Event
import SDL.Geometry
import SDL.Loading
import DrawTile
import Grid
import SlidingGrid

data World = World { gameOver :: Bool
                   , grid :: Maybe (TileZipper ()) }

tileSize :: Point CInt
tileSize = (20, 20)

drawState :: SDL.T.Renderer -> SDL.T.Rect -> [Asset] -> World -> IO ()
drawState r fullWindow assets state = case grid state of
  Nothing -> clearScreen r
  Just z -> withBlankScreen r $ drawTiles r tileSize (0, 0) z

updateState :: Input -> World -> World
updateState (Just (SDL.T.QuitEvent _ _)) state = state { gameOver = True }
updateState (Just (SDL.T.KeyboardEvent evtType _ _ _ _ keysym)) state =
  if evtType == SDL.E.SDL_KEYDOWN
  then modifyState state keysym
  else state
updateState _ state = state

modifyState :: World -> SDL.T.Keysym -> World
modifyState state keysym = case getKey keysym of
  _ -> state

runUntilComplete :: (Monad m) => m World -> m ()
runUntilComplete game = game >>= \state -> unless (gameOver state) $ runUntilComplete game


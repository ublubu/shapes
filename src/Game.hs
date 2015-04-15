module Game where

import qualified Graphics.UI.SDL.Video as SDL.V
import qualified Graphics.UI.SDL.Enum as SDL.E
import qualified Graphics.UI.SDL.Types as SDL.T
import qualified Graphics.UI.SDL.Basic as SDL.B
import qualified Graphics.UI.SDL.Timer as SDL.Timer
import qualified Graphics.UI.SDL.Event as SDL.Event
import Control.Monad
import SDL.Draw
import SDL.Event
import SDL.Geometry
import SDL.Loading

data World = World { gameover :: Bool, degrees :: Int, flipType :: SDL.E.RendererFlip }

drawState :: SDL.T.Renderer -> SDL.T.Rect -> [Asset] -> World -> IO ()
drawState r fullWindow assets state = withBlankScreen r $ drawAsset r asset translate (degrees state) (flipType state)
  where asset = head assets
        translate = (`centredOn` fullWindow)

updateState :: Input -> World -> World
updateState (Just (SDL.T.QuitEvent _ _)) state = state { gameover = True }
updateState (Just (SDL.T.KeyboardEvent evtType _ _ _ _ keysym)) state =
  if evtType == SDL.E.SDL_KEYDOWN
  then modifyState state keysym
  else state
updateState _ state = state

modifyState :: World -> SDL.T.Keysym -> World
modifyState state keysym = case getKey keysym of
  Q -> state { flipType = SDL.E.SDL_FLIP_HORIZONTAL }
  W -> state { flipType = SDL.E.SDL_FLIP_NONE }
  E -> state { flipType = SDL.E.SDL_FLIP_VERTICAL }
  A -> state { degrees = degrees state - 15 }
  D -> state { degrees = degrees state + 15 }
  _ -> state

runUntilComplete :: (Monad m) => m World -> m ()
runUntilComplete game = game >>= \state -> unless (gameover state) $ runUntilComplete game


{-# LANGUAGE PatternSynonyms #-}

module Game where

import qualified Graphics.UI.SDL.Video as SDL.V
import qualified Graphics.UI.SDL.Enum as SDL.E
import qualified Graphics.UI.SDL.Types as SDL.T
import qualified Graphics.UI.SDL.Basic as SDL.B
import qualified Graphics.UI.SDL.Timer as SDL.Timer
import qualified Graphics.UI.SDL.Event as SDL.Event
import Control.Monad
import Data.Word
import Data.Maybe
import Foreign.C.Types
import SDL.Draw
import SDL.Event
import SDL.Geometry
import SDL.Loading
import DrawTile
import SmoothSlidingGrid
import SlidingGrid
import Grid
import GHC.Int

data InputState = InputState { mouseButtonDown :: Maybe GeomPoint
                             , mousePosition :: GeomPoint } deriving Show

data World = World { gameOver :: Bool
                   , grid :: TileZipper ()
                   , worldInput :: InputState } deriving Show

mouseSlide :: World -> Maybe GeomPoint
mouseSlide state = do
  x <- mouseButtonDown inputState
  return (x' - x)
  where inputState = worldInput state
        x' = mousePosition inputState

tileSize :: Point CInt
tileSize = (20, 20)

drawState :: SDL.T.Renderer -> SDL.T.Rect -> [Asset] -> World -> IO ()
drawState r fullWindow assets state =
  withBlankScreen r $ drawTiles r tileSize (0, 0) g
  where g = toSmoothSliding (grid state)

updateState :: Input -> World -> World
updateState (Just (SDL.T.QuitEvent _ _)) state = state { gameOver = True }
updateState (Just (SDL.T.KeyboardEvent evtType _ _ _ _ keysym)) state =
  if evtType == SDL.E.SDL_KEYDOWN
  then modifyState state keysym
  else state
updateState (Just (SDL.T.MouseMotionEvent { SDL.T.mouseMotionEventX = x
                                          , SDL.T.mouseMotionEventY = y })) state =
  state { worldInput = applyMouseMotion x y (worldInput state)}
updateState (Just (SDL.T.MouseButtonEvent { SDL.T.eventType = evtType
                                          , SDL.T.mouseButtonEventButton = button
                                          , SDL.T.mouseButtonEventX = x
                                          , SDL.T.mouseButtonEventY = y })) state =
  state { worldInput = f button x y (worldInput state) }
  where f = case evtType of
          SDL.E.SDL_MOUSEBUTTONDOWN -> applyMouseButtonDown
          SDL.E.SDL_MOUSEBUTTONUP -> applyMouseButtonUp

updateState _ state = state

modifyState :: World -> SDL.T.Keysym -> World
modifyState state keysym = case getKey keysym of
  _ -> state

applyMouseMotion :: Int32 -> Int32 -> InputState -> InputState
applyMouseMotion x y state = state { mousePosition = toGeomPointInt (x, y) }

applyMouseButtonDown :: Word8 -> Int32 -> Int32 -> InputState -> InputState
applyMouseButtonDown button x y state = if button == SDL.E.SDL_BUTTON_LEFT
                                        then state { mouseButtonDown = Just pos
                                                   , mousePosition = pos }
                                        else state { mousePosition = pos }
  where pos = toGeomPointInt (x, y)

applyMouseButtonUp :: Word8 -> Int32 -> Int32 -> InputState -> InputState
applyMouseButtonUp button x y state = if button == SDL.E.SDL_BUTTON_LEFT
                                        then state { mouseButtonDown = Nothing
                                                   , mousePosition = pos }
                                        else state { mousePosition = pos }
  where pos = toGeomPointInt (x, y)

runUntilComplete :: (Monad m) => m World -> m ()
runUntilComplete game = game >>= \state -> unless (gameOver state) $ runUntilComplete game


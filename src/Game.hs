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
import Debug.Trace
import Foreign.C.Types
import GHC.Int
import SDL.Draw
import SDL.Event
import SDL.Geometry
import SDL.Loading
import Drag
import DrawTile
import SmoothSlidingGrid
import SlidingGrid
import Grid
import GameInput
import GameState
import GameTile
import Utils.Utils

data World = World { gameOver :: Bool
                   , remainingLevels :: [GridState]
                   , gridDrawInfo :: GridDrawInfo CInt
                   , gridState :: GridState
                   , gridInput :: GridInput } deriving Show

drawState :: SDL.T.Renderer -> SDL.T.Rect -> [Asset] -> World -> IO ()
drawState r fullWindow assets state =
  withBlankScreen r $ drawGrid r gdi gi gs
  where gs = gridState state
        gi = gridInput state
        gdi = gridDrawInfo state

updateState :: Input -> World -> World
updateState input = maybeAdvanceLevel . updateState_ input

updateState_ :: Input -> World -> World
updateState_ (Just (SDL.T.QuitEvent _ _)) state = state { gameOver = True }
updateState_ (Just (SDL.T.KeyboardEvent evtType _ _ _ _ keysym)) state =
  if evtType == SDL.E.SDL_KEYDOWN
  then modifyState state keysym
  else state
updateState_ (Just (SDL.T.MouseMotionEvent { SDL.T.mouseMotionEventX = x
                                          , SDL.T.mouseMotionEventY = y })) state =
  applyMouseMotion x y state
updateState_ (Just (SDL.T.MouseButtonEvent { SDL.T.eventType = evtType
                                          , SDL.T.mouseButtonEventButton = button
                                          , SDL.T.mouseButtonEventX = x
                                          , SDL.T.mouseButtonEventY = y })) state =
  f button x y state
  where f = case evtType of
          SDL.E.SDL_MOUSEBUTTONDOWN -> applyMouseButtonDown
          SDL.E.SDL_MOUSEBUTTONUP -> applyMouseButtonUp
updateState_ _ state = state

modifyState :: World -> SDL.T.Keysym -> World
modifyState state keysym = case getKey keysym of
  _ -> state

applyMouseMotion :: Int32 -> Int32 -> World -> World
applyMouseMotion x y state = case gridDrag inputState of
  -- update the end of the current drag (if we are dragging)
  Nothing -> state
  Just (click, _) -> state { gridState = gs'
                           , gridInput = inputState { gridDrag = Just drag'
                                                    , gridPartialMove = partialMove }}
    where (drag', partialMove, gs') = completelyApplyDrag drawInfo drag gs
          drawInfo = fmap fromIntegral (gridDrawInfo state)
          drag = (click, pairMap fromIntegral (x, y))
          gs = gridState state
  where inputState = gridInput state

applyMouseButtonDown :: Word8 -> Int32 -> Int32 -> World -> World
applyMouseButtonDown button x y state =
  if button == SDL.E.SDL_BUTTON_LEFT
  then state { gridInput = (gridInput state) {gridDrag = Just (pos, pos) } } -- start new drag
  else state -- we drag using the left button
  where pos = pairMap fromIntegral (x, y)

applyMouseButtonUp :: Word8 -> Int32 -> Int32 -> World -> World
applyMouseButtonUp button x y state =
  if button == SDL.E.SDL_BUTTON_LEFT
  then state { gridInput = defaultGridInput
             , gridState = gs'} -- release the current drag
  else state -- we drag using left button
  where pos = toGeomPointInt (x, y)
        input = gridInput state
        drawInfo = fmap fromIntegral (gridDrawInfo state)
        move = convertPartialMove drawInfo =<< gridPartialMove input
        gs = gridState state
        gs' = maybe gs (`applyFullMove` gs) move

maybeAdvanceLevel :: World -> World
maybeAdvanceLevel world@(World _ levels _ gs _) =
  if levelComplete gs
  then case levels of
    [] -> world { gameOver = True -- all levels completed - end the game
                , gridInput = defaultGridInput
                , remainingLevels = [] }
    (x:xs) -> world { remainingLevels = xs -- advance to the next level
                    , gridInput = defaultGridInput
                    , gridState = x }
  else world

runUntilComplete :: (Monad m) => m World -> m ()
runUntilComplete game = game >>= \state -> unless (gameOver state) $ runUntilComplete game

instance Draggable (GridState) where
  setCoord coord gs@(GridState _ _ tiles) = do
    tiles' <- Grid.moveTo coord tiles
    return $ gs { gridTiles = tiles' }
  applyMove dir gs@(GridState player _ tiles) =
    trace "applying a move"
    (if player == gridCoord tiles then movePlayer dir gs
     else gs { gridTiles = fromMaybe tiles $ slide_ dir tiles })
  checkMove dir (GridState player _ tiles) = canMove player dir tiles


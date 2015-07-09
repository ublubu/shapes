module SideScroller.Game where

import qualified Graphics.UI.SDL.Video as SDL.V
import qualified Graphics.UI.SDL.Enum as SDL.E
import qualified Graphics.UI.SDL.Types as SDL.T
import qualified Graphics.UI.SDL.Basic as SDL.B
import qualified Graphics.UI.SDL.Timer as SDL.Timer
import qualified Graphics.UI.SDL.Event as SDL.Event
--import qualified Graphics.UI.SDL.Image as Image
import Control.Monad
import GHC.Word
import Foreign.C.Types
import SDL.Draw
import SDL.Error
import SDL.Event
import SDL.Init
import SDL.Loading
import SDL.Geometry
import GameLoop
import Geometry
import Utils.Utils
import SideScroller.Draw
import SideScroller.GameState
import SideScroller.RectangularWorld

type GameFrame = (Word32, GameState)

data AppState = AppState { fullFrame :: GameFrame
                         , interpFrame :: GameFrame
                         , appFinished :: Bool }

defaultAppState :: Word32 -> AppState
defaultAppState t = AppState { fullFrame = (t, defaultGameState)
                             , interpFrame = (t, defaultGameState)
                             , appFinished = False }

frameMillis :: Num a => a
frameMillis = 100

millisPerSecond :: Num a => a
millisPerSecond = 1000

frameSeconds :: Double
frameSeconds = frameMillis / millisPerSecond

main :: Pair CInt -> SDL.T.Renderer -> IO ()
main window r = do
  t0 <- SDL.Timer.getTicks
  runUntil (defaultAppState t0) appFinished (updater $ gameStep r)

gameStep :: SDL.T.Renderer -> AppState -> Word32 -> IO AppState
gameStep r s0 t1 = do
  events <- flushEvents
  let s = foldl handleEvent s0 events
      s' = simulate s t1
  draw r $ (snd . interpFrame) s'
  return s'

handleEvent :: AppState -> SDL.T.Event -> AppState
handleEvent s0 (SDL.T.QuitEvent _ _) = s0 { appFinished = True }
handleEvent s0 _ = s0

simulate :: AppState -> Word32 -> AppState
simulate s0 t1 = s0 { fullFrame = (t', g')
                    , interpFrame = (t'', g'') }
  where (t0, g0) = fullFrame s0
        (steps, remaining) = timeSteps_ (t1 - t0) frameMillis
        t' = t0 + (steps * frameMillis)
        t'' = t1
        g' = g0 { gameBoxes = advanceSteps (gameBoxes g0) frameSeconds (fromIntegral steps) }
        g'' = g' { gameBoxes = interpStep (gameBoxes g0) (fromIntegral remaining / millisPerSecond) }



module GameInit where

import qualified Graphics.UI.SDL.Video as SDL.V
import qualified Graphics.UI.SDL.Enum as SDL.E
import qualified Graphics.UI.SDL.Types as SDL.T
import qualified Graphics.UI.SDL.Basic as SDL.B
import qualified Graphics.UI.SDL.Timer as SDL.Timer
import qualified Graphics.UI.SDL.Event as SDL.Event
--import qualified Graphics.UI.SDL.Image as Image
import Foreign.C.Types
import SDL.Draw
import SDL.Error
import SDL.Event
import SDL.Init
import SDL.Loading
import SDL.Geometry
import Geometry
import Utils.Utils

runMain :: String -> Pair CInt -> (SDL.T.Renderer -> IO ()) -> IO ()
runMain windowTitle (Pair screenWidth screenHeight) main = do
  initializeSDL [SDL.E.SDL_INIT_VIDEO] >>= catchRisky
  --Image.imgInit [Image.InitPNG]

  setHint "SDL_RENDER_SCALE_QUALITY" "1" >>= logWarning
  window <- createWindow windowTitle screenWidth screenHeight >>= catchRisky
  renderer <- createRenderer window (-1) [SDL.E.SDL_RENDERER_ACCELERATED] >>= catchRisky

  main renderer

  SDL.V.destroyRenderer renderer
  SDL.V.destroyWindow window
  SDL.B.quit
  --Image.imgQuit


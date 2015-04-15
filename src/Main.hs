import qualified Graphics.UI.SDL.Video as SDL.V
import qualified Graphics.UI.SDL.Enum as SDL.E
import qualified Graphics.UI.SDL.Types as SDL.T
import qualified Graphics.UI.SDL.Basic as SDL.B
import qualified Graphics.UI.SDL.Timer as SDL.Timer
import qualified Graphics.UI.SDL.Event as SDL.Event
import qualified Graphics.UI.SDL.Image as Image
import Control.Monad
import Control.Monad.State hiding (state)
import Data.Bits
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.Word
import SDL.Draw
import SDL.Error
import SDL.Event
import SDL.Init
import SDL.Loading
import Utils.Utils
import Game

---- Config ----

windowTitle :: String
windowTitle = "Tile Rider"

screenWidth :: CInt
screenWidth = 640

screenHeight :: CInt
screenHeight = 480

fullWindow :: SDL.T.Rect
fullWindow = SDL.T.Rect {
  SDL.T.rectX = 0,
  SDL.T.rectY = 0,
  SDL.T.rectW = screenWidth,
  SDL.T.rectH = screenHeight }

initialState :: World
initialState = World { gameover = False, degrees = 0, flipType = SDL.E.SDL_FLIP_NONE }

---- Application ----

main :: IO ()
main = do
    initializeSDL [SDL.E.SDL_INIT_VIDEO] >>= catchRisky
    Image.imgInit [Image.InitPNG]

    setHint "SDL_RENDER_SCALE_QUALITY" "1" >>= logWarning
    window <- createWindow windowTitle screenWidth screenHeight >>= catchRisky
    renderer <- createRenderer window (-1) [SDL.E.SDL_RENDERER_ACCELERATED] >>= catchRisky

    asset <- loadTexture renderer "./assets/blop-nar.png" >>= catchRisky

    let inputSource = pollEvent `into` updateState
    let pollDraw = inputSource ~>~ drawState renderer fullWindow [asset]
    runStateT (runUntilComplete pollDraw) initialState

    freeAssets [asset]
    SDL.V.destroyRenderer renderer
    SDL.V.destroyWindow window
    SDL.B.quit
    Image.imgQuit

drawAll :: SDL.T.Renderer -> IO ()
drawAll renderer = do
    clearScreen renderer
    withColor Red >> fillRectangle' innerRect
    withColor Green >> drawRectangle' outerRect
    withColor Blue >> drawLine' (0, screenHeight `div` 2) (screenWidth, screenHeight `div` 2)
    withColor Yellow >> mapM_ (\y -> drawDot' (screenWidth `div` 2, y)) [ 0, 4 .. screenHeight ]
    SDL.V.renderPresent renderer

    where innerRect = SDL.T.Rect { SDL.T.rectX = screenWidth `div` 4, SDL.T.rectY = screenHeight `div` 4, SDL.T.rectW = screenWidth `div` 2, SDL.T.rectH = screenHeight `div` 2 }
          outerRect = SDL.T.Rect { SDL.T.rectX = screenWidth `div` 6, SDL.T.rectY = screenHeight `div` 6, SDL.T.rectW = 2 * screenWidth `div` 3, SDL.T.rectH = 2 * screenHeight `div` 3 }
          withColor = setColor renderer
          fillRectangle' = fillRectangle renderer
          drawRectangle' = drawRectangle renderer
          drawLine' = drawLine renderer
          drawDot' = drawDot renderer

import qualified Graphics.UI.SDL.Video as SDL.Video
import qualified Graphics.UI.SDL.Enum as SDL.Enum
import qualified Graphics.UI.SDL.Types as SDL.Types
import qualified Graphics.UI.SDL.Basic as SDL.Basic
import qualified Graphics.UI.SDL.Timer as SDL.Timer
import Control.Monad
import Data.Bits
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import GHC.Word

type Risky a = Either String a

lessonTitle :: String
lessonTitle = "lesson01"

screenWidth :: CInt
screenWidth = 640

screenHeight :: CInt
screenHeight = 480

main :: IO ()
main = do
  initializeSDL [SDL.Enum.SDL_INIT_VIDEO] >>= either throwSDLError return
  window <- createWindow lessonTitle >>= either throwSDLError return
  
  screenSurface <- SDL.Video.getWindowSurface window
  pixelFormat <- SDL.Types.surfaceFormat `applyToPointer` screenSurface
  color <- SDL.Video.mapRGB pixelFormat 0xFF 0xFF 0xFF
  
  SDL.Video.fillRect screenSurface nullPtr color
  SDL.Video.updateWindowSurface window
  SDL.Timer.delay 2000

  SDL.Video.destroyWindow window
  SDL.Video.destroyWindow window

initializeSDL :: [Word32] -> IO (Risky ())
initializeSDL flags = do
  initSuccess <- SDL.Basic.init $ foldl (.|.) 0 flags
  return $ if initSuccess < 0 then Left "SDL could not initialize" else Right ()

createWindow :: String -> IO (Risky SDL.Types.Window)
createWindow windowTitle = withCAString windowTitle $ \title -> do
  window <- SDL.Video.createWindow title SDL.Enum.SDL_WINDOWPOS_UNDEFINED
            SDL.Enum.SDL_WINDOWPOS_UNDEFINED screenWidth screenHeight SDL.Enum.SDL_WINDOW_SHOWN
  return $ if window == nullPtr then Left "Window could not be created!" else Right window

throwSDLError :: String -> IO a
throwSDLError message = do
  errorString <- SDL.Basic.getError >>= peekCString
  fail (message ++ " SDL_Error: " ++ errorString)

applyToPointer :: (Storable a) => (a -> b) -> Ptr a -> IO b
applyToPointer operation pointer = liftM operation $ peek pointer


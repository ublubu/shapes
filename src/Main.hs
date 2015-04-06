import qualified Graphics.UI.SDL.Video as SDL.V
import qualified Graphics.UI.SDL.Enum as SDL.E
import qualified Graphics.UI.SDL.Types as SDL.T
import qualified Graphics.UI.SDL.Basic as SDL.B
import qualified Graphics.UI.SDL.Timer as SDL.Timer
import qualified Graphics.UI.SDL.Event as SDL.Event
import Control.Monad
import Data.Bits
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.Word

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

---- Application ----

main :: IO ()
main = do
    initializeSDL [SDL.E.SDL_INIT_VIDEO] >>= catchRisky

    setHint "SDL_RENDER_SCALE_QUALITY" "1" >>= logWarning
    window <- createWindow windowTitle >>= catchRisky
    renderer <- createRenderer window (-1) [SDL.E.SDL_RENDERER_ACCELERATED] >>= catchRisky
    SDL.V.setRenderDrawColor renderer 0xFF 0xFF 0xFF 0xFF

    repeatUntilComplete $ drawAll renderer >> handle pollEvent

    SDL.V.destroyRenderer renderer
    SDL.V.destroyWindow window
    SDL.B.quit

---- Initialization ----

initializeSDL :: [Word32] -> IO (Risky CInt)
initializeSDL flags = do
    result <- SDL.B.init $ foldl (.|.) 0 flags
    return $ if result < 0 then Left "SDL could not initialize!" else Right result


createWindow :: String -> IO (Risky SDL.T.Window)
createWindow windowTitle = withCAString windowTitle $ \title -> do
    window <- SDL.V.createWindow title SDL.E.SDL_WINDOWPOS_UNDEFINED SDL.E.SDL_WINDOWPOS_UNDEFINED screenWidth screenHeight SDL.E.SDL_WINDOW_SHOWN
    return $ if window == nullPtr then Left "Window could not be created!" else Right window


createRenderer :: SDL.T.Window -> CInt -> [Word32] -> IO (Risky SDL.T.Renderer)
createRenderer window index flags = do
    renderer <- SDL.V.createRenderer window index $ foldl (.|.) 0 flags
    return $ if renderer == nullPtr then Left "Renderer could not be created!" else Right renderer


setHint :: String -> String -> IO (Risky Bool)
setHint hint value = do
    result <- withCAString2 hint value SDL.B.setHint
    return $ if not result then Left "Warning: Linear texture filtering not enabled!" else Right result


---- Drawing ----

data Colour = White | Red | Blue | Green | Yellow


draw :: SDL.T.Renderer -> SDL.T.Texture -> IO ()
draw renderer texture = do
    SDL.V.renderClear renderer
    SDL.V.renderCopy renderer texture nullPtr nullPtr
    SDL.V.renderPresent renderer


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


clearScreen :: SDL.T.Renderer -> IO CInt
clearScreen renderer = do
    setColor renderer White
    SDL.V.renderClear renderer


fillRectangle :: SDL.T.Renderer -> SDL.T.Rect -> IO CInt
fillRectangle renderer shape = with shape $ SDL.V.renderFillRect renderer


drawRectangle :: SDL.T.Renderer -> SDL.T.Rect -> IO CInt
drawRectangle renderer shape = with shape $ SDL.V.renderDrawRect renderer


drawLine :: SDL.T.Renderer -> (CInt, CInt) -> (CInt, CInt) -> IO CInt
drawLine renderer (ox, oy) (tx, ty) = SDL.V.renderDrawLine renderer ox oy tx ty


drawDot :: SDL.T.Renderer -> (CInt, CInt) -> IO CInt
drawDot renderer (x, y) = SDL.V.renderDrawPoint renderer x y


setColor :: SDL.T.Renderer -> Colour -> IO CInt
setColor renderer White  = SDL.V.setRenderDrawColor renderer 0xFF 0xFF 0xFF 0xFF
setColor renderer Red    = SDL.V.setRenderDrawColor renderer 0xFF 0x00 0x00 0xFF
setColor renderer Green  = SDL.V.setRenderDrawColor renderer 0x00 0xFF 0x00 0xFF
setColor renderer Blue   = SDL.V.setRenderDrawColor renderer 0x00 0x00 0xFF 0xFF
setColor renderer Yellow = SDL.V.setRenderDrawColor renderer 0xFF 0xFF 0x00 0xFF


---- Event Handling ----

handle :: IO (Maybe SDL.T.Event) -> IO Bool
handle stream = do
    maybeEvent <- stream
    case maybeEvent of
        Nothing -> return False

        Just (SDL.T.QuitEvent _ _) -> return True

        _ -> return False


pollEvent :: IO (Maybe SDL.T.Event)
pollEvent = alloca $ \pointer -> do
    status <- SDL.Event.pollEvent pointer

    if status == 1
        then maybePeek peek pointer
        else return Nothing


---- Error Handling ----

type Risky a = Either String a


catchRisky :: Risky a -> IO a
catchRisky = either throwSDLError return


logWarning :: Risky Bool -> IO Bool
logWarning = either (\x -> print x >> return False) return


throwSDLError :: String -> IO a
throwSDLError message = do
    errorString <- SDL.B.getError >>= peekCString
    fail (message ++ " SDL_Error: " ++ errorString)


---- Utils ----

withCAString2 :: String -> String -> (CString -> CString -> IO a) -> IO a
withCAString2 a b op = withCAString a $ \a' -> withCAString b $ op a'


repeatUntilComplete :: IO Bool -> IO ()
repeatUntilComplete operation = do
    complete <- operation
    unless complete $ repeatUntilComplete operation


applyToPointer :: (Storable a) => (a -> b) -> Ptr a -> IO b
applyToPointer operation pointer = liftM operation $ peek pointer

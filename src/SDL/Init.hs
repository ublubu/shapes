module SDL.Init where

import qualified Graphics.UI.SDL.Video as SDL.V
import qualified Graphics.UI.SDL.Enum as SDL.E
import qualified Graphics.UI.SDL.Types as SDL.T
import qualified Graphics.UI.SDL.Basic as SDL.B
import qualified Graphics.UI.SDL.Image as Image
import Data.Bits
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import GHC.Word
import SDL.Error
import Utils.Utils

initializeSDL :: [Word32] -> IO (Risky CInt)
initializeSDL flags = do
    result <- SDL.B.init $ foldl (.|.) 0 flags
    return $ if result < 0 then Left "SDL could not initialize!" else Right result


createWindow :: String -> CInt -> CInt -> IO (Risky SDL.T.Window)
createWindow windowTitle screenWidth screenHeight = withCAString windowTitle $ \title -> do
  window <- SDL.V.createWindow title SDL.E.SDL_WINDOWPOS_UNDEFINED SDL.E.SDL_WINDOWPOS_UNDEFINED
    screenWidth screenHeight SDL.E.SDL_WINDOW_SHOWN
  return $ if window == nullPtr then Left "Window could not be created!" else Right window


createRenderer :: SDL.T.Window -> CInt -> [Word32] -> IO (Risky SDL.T.Renderer)
createRenderer window index flags = do
    renderer <- SDL.V.createRenderer window index $ foldl (.|.) 0 flags
    return $ if renderer == nullPtr then Left "Renderer could not be created!" else Right renderer


setHint :: String -> String -> IO (Risky Bool)
setHint hint value = do
    result <- withCAString2 hint value SDL.B.setHint
    return $ if not result then Left "Warning: Linear texture filtering not enabled!" else Right result

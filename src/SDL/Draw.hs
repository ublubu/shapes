module SDL.Draw where

import qualified Graphics.UI.SDL.Video as SDL.V
import qualified Graphics.UI.SDL.Types as SDL.T
import qualified Graphics.UI.SDL.Enum as SDL.E
import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Ptr
import SDL.Loading
import SDL.Geometry
import SDL.Error
import Utils.Utils

data Colour = White | Red | Blue | Green | Yellow

draw :: SDL.T.Renderer -> SDL.T.Texture -> IO ()
draw renderer texture = do
    _ <- SDL.V.renderClear renderer
    _ <- SDL.V.renderCopy renderer texture nullPtr nullPtr
    SDL.V.renderPresent renderer


clearScreen :: SDL.T.Renderer -> IO CInt
clearScreen renderer = do
    _ <- setColor renderer White
    SDL.V.renderClear renderer


fillRectangle :: SDL.T.Renderer -> SDL.T.Rect -> IO CInt
fillRectangle renderer shape = with shape $ SDL.V.renderFillRect renderer


drawRectangle :: SDL.T.Renderer -> SDL.T.Rect -> IO CInt
drawRectangle renderer shape = with shape $ SDL.V.renderDrawRect renderer


drawLine :: SDL.T.Renderer -> (CInt, CInt) -> (CInt, CInt) -> IO CInt
drawLine renderer (ox, oy) (tx, ty) = SDL.V.renderDrawLine renderer ox oy tx ty


drawDot :: SDL.T.Renderer -> (CInt, CInt) -> IO CInt
drawDot renderer (x, y) = SDL.V.renderDrawPoint renderer x y


drawAsset :: SDL.T.Renderer -> Asset -> (SDL.T.Rect -> SDL.T.Rect) -> Int -> SDL.E.RendererFlip -> IO ()
drawAsset r asset translate degrees flip = with2 mask target $ \mask' target' ->
  SDL.V.renderCopyEx r texture mask' target' degrees' nullPtr flip >>= catchErrorCode "renderCopyEx failed"
  where (texture, w, h) = asset
        mask = toRect 0 0 w h
        target = translate mask
        degrees' = fromIntegral degrees


setColor :: SDL.T.Renderer -> Colour -> IO CInt
setColor renderer White  = SDL.V.setRenderDrawColor renderer 0xFF 0xFF 0xFF 0xFF
setColor renderer Red    = SDL.V.setRenderDrawColor renderer 0xFF 0x00 0x00 0xFF
setColor renderer Green  = SDL.V.setRenderDrawColor renderer 0x00 0xFF 0x00 0xFF
setColor renderer Blue   = SDL.V.setRenderDrawColor renderer 0x00 0x00 0xFF 0xFF
setColor renderer Yellow = SDL.V.setRenderDrawColor renderer 0xFF 0xFF 0x00 0xFF

withBlankScreen :: SDL.T.Renderer -> IO a -> IO ()
withBlankScreen r operation = do
  setColor r White
  SDL.V.renderClear r
  operation
  SDL.V.renderPresent r


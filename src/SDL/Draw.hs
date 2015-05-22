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

data Colour = White | Red | Blue | Green | Yellow | Black

drawTextureStretch :: SDL.T.Renderer -> SDL.T.Texture -> IO ()
drawTextureStretch renderer texture = do
  clearScreen renderer
  SDL.V.renderCopy renderer texture nullPtr nullPtr >>=
    catchErrorCode "failed to copy texture to target"
  SDL.V.renderPresent renderer

clearScreen :: SDL.T.Renderer -> IO ()
clearScreen renderer = do
  setColor renderer White
  SDL.V.renderClear renderer >>= catchErrorCode "failed to clear"

fillRectangle :: SDL.T.Renderer -> SDL.T.Rect -> IO ()
fillRectangle renderer shape = catchErrorCode "failed to fill rectangle" =<<
                               (with shape $ SDL.V.renderFillRect renderer)

drawRectangle :: SDL.T.Renderer -> SDL.T.Rect -> IO ()
drawRectangle renderer shape = catchErrorCode "failed to draw rectangle" =<<
                               (with shape $ SDL.V.renderDrawRect renderer)

drawLine :: SDL.T.Renderer -> (CInt, CInt) -> (CInt, CInt) -> IO ()
drawLine renderer (ox, oy) (tx, ty) =
  catchErrorCode "failed to draw rectangle" =<<
  SDL.V.renderDrawLine renderer ox oy tx ty

drawDot :: SDL.T.Renderer -> (CInt, CInt) -> IO ()
drawDot renderer (x, y) = catchErrorCode "failed to draw dot" =<<
                          SDL.V.renderDrawPoint renderer x y

drawAsset :: SDL.T.Renderer -> Asset -> (SDL.T.Rect -> SDL.T.Rect) -> Int -> SDL.E.RendererFlip -> IO ()
drawAsset r asset translate degrees flip = with2 mask target $ \mask' target' ->
  SDL.V.renderCopyEx r texture mask' target' degrees' nullPtr flip >>= catchErrorCode "renderCopyEx failed"
  where (texture, w, h) = asset
        mask = toRect 0 0 w h
        target = translate mask
        degrees' = fromIntegral degrees

setColorUnsafe :: SDL.T.Renderer -> Colour -> IO CInt
setColorUnsafe renderer White  = SDL.V.setRenderDrawColor renderer 0xFF 0xFF 0xFF 0xFF
setColorUnsafe renderer Red    = SDL.V.setRenderDrawColor renderer 0xFF 0x00 0x00 0xFF
setColorUnsafe renderer Green  = SDL.V.setRenderDrawColor renderer 0x00 0xFF 0x00 0xFF
setColorUnsafe renderer Blue   = SDL.V.setRenderDrawColor renderer 0x00 0x00 0xFF 0xFF
setColorUnsafe renderer Yellow = SDL.V.setRenderDrawColor renderer 0xFF 0xFF 0x00 0xFF
setColorUnsafe renderer Black = SDL.V.setRenderDrawColor renderer 0x00 0x00 0x00 0xFF

setColor :: SDL.T.Renderer -> Colour -> IO ()
setColor = (catchErrorCode "failed to set color" =<<) <. setColorUnsafe

withBlankScreen :: SDL.T.Renderer -> IO () -> IO ()
withBlankScreen r operation = do
  clearScreen r
  operation
  SDL.V.renderPresent r


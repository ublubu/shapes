module SDL.Loading where

import qualified Graphics.UI.SDL.Video as SDL.V
import qualified Graphics.UI.SDL.Enum as SDL.E
import qualified Graphics.UI.SDL.Types as SDL.T
import qualified Graphics.UI.SDL.Image as Image
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import GHC.Word
import SDL.Error
import Utils.Utils

type Asset = (SDL.T.Texture, CInt, CInt)


freeAssets :: [Asset] -> IO ()
freeAssets = mapM_ (SDL.V.destroyTexture . first)
  where first (a, _, _) = a


convertSurface :: Ptr SDL.T.Surface -> Ptr SDL.T.PixelFormat -> Word32 -> IO (Risky (Ptr SDL.T.Surface))
convertSurface surface format flags = do
    optimizedSurface <- SDL.V.convertSurface surface format flags
    return $ if optimizedSurface == nullPtr then Left "Unable to optimize image!" else Right optimizedSurface


textureDimensions :: SDL.T.Texture -> IO (Risky (CInt, CInt))
textureDimensions t = with4 SDL.E.SDL_PIXELFORMAT_UNKNOWN SDL.E.SDL_TEXTUREACCESS_STATIC 0 0 f
  where f format access w h = do
          result <- SDL.V.queryTexture t format access w h
          if result < 0
            then return $ Left "failed to query texture"
            else do
            width <- peek w
            height <- peek h
            return $ Right (width, height)


loadTexture :: SDL.T.Renderer -> String -> IO (Risky Asset)
loadTexture renderer path = do
    loadedTexture <- Image.imgLoadTexture renderer path >>= catchRisky
    (width, height) <- textureDimensions loadedTexture >>= catchRisky
    return $ if loadedTexture == nullPtr then Left "failed to load texture image" else Right (loadedTexture, width, height)


createTextureFromSurface :: SDL.T.Renderer -> Ptr SDL.T.Surface -> IO (Risky SDL.T.Texture)
createTextureFromSurface renderer surface = do
    result <- SDL.V.createTextureFromSurface renderer surface
    return $ if result == nullPtr then Left "Unable to create texture" else Right result


renderTexture :: SDL.T.Renderer -> SDL.T.Texture -> SDL.T.Rect -> SDL.T.Rect -> IO CInt
renderTexture renderer texture renderMask renderQuad = with2 renderMask renderQuad $ SDL.V.renderCopy renderer texture

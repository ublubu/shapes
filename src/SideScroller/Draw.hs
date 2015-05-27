module SideScroller.Draw where

import Control.Monad
import qualified Graphics.UI.SDL.Types as SDL.T
import SideScroller.Calculus
import Geometry
import qualified SDL.Geometry as SDL.G
import SDL.Draw
import SideScroller.RectangularWorld
import SideScroller.GameState

toRect :: (Integral a) => Transform (Pair a) -> SDL.T.Rect
toRect (Transform (Pair w h) (Pair x y)) = SDL.G.toRect x y w h

drawBox :: (RealFrac a) => SDL.T.Renderer -> Transform (Pair a) -> Box (Pair a) -> IO ()
drawBox r viewTrans (Box boxTrans _) = do
  setColor r Black
  fillRectangle r target
  where target = toRect $ fmap ffloor (joinTrans (fmap baseValue boxTrans) viewTrans)

draw_ :: (RealFrac a) => SDL.T.Renderer -> Transform (Pair a) -> Boxes a (Pair a) -> IO ()
draw_ r viewTrans (Boxes bs _) = msum $ fmap (drawBox r viewTrans) bs

draw :: SDL.T.Renderer -> GameState -> IO ()
draw r state = withBlankScreen r $ draw_ r viewTrans bs
  where bs = gameBoxes state
        viewTrans = cameraTransform (gameCamera state)

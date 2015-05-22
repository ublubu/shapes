module SideScroller.Draw where

import qualified Graphics.UI.SDL.Types as SDL.T
import SideScroller.Calculus
import Geometry
import qualified SDL.Geometry as SDL.G
import SDL.Draw
import SideScroller.RectangularWorld

toRect :: (Integral a) => Transform (Pair a) -> SDL.T.Rect
toRect (Transform (Pair w h) (Pair x y)) = SDL.G.toRect x y w h

drawBox :: (RealFrac a) => SDL.T.Renderer -> Transform (Pair a) -> Box (Pair a) -> IO ()
drawBox r viewTrans (Box boxTrans _) = do
  setColor r Black
  fillRectangle r target
  where target = toRect $ fmap ffloor (joinTrans boxTrans viewTrans)


--draw :: (RealFrac a) => SDL.T.Renderer -> Transform (Pair a) -> Boxes a (Pair a) -> IO ()
--draw r window viewTrans (Boxes bs _) = 

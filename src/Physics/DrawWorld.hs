module Physics.DrawWorld where

import Control.Lens hiding (transform)
import qualified Graphics.UI.SDL.Video as SDL.V
import qualified Graphics.UI.SDL.Types as SDL.T
import qualified Graphics.UI.SDL.Enum as SDL.E
import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Ptr
import GHC.Word
import Linear.Affine
import Linear.V2
import qualified SDL.Draw as D
import SDL.Loading
import SDL.Geometry
import SDL.Error
import Utils.Utils
import Physics.Constraint
import Physics.Draw
import Physics.Transform
import Physics.Linear
import Physics.Geometry
import Physics.World
import Geometry

drawObj :: (RealFrac a, Floating a) => SDL.T.Renderer -> WorldTransform a -> PhysicalObj a -> IO ()
drawObj r vt o = drawConvexHull r hull
  where hull = transform vt . transform t $ (o ^. physObjHull)
        t = toTransform (o ^. physObjPos) (o ^. physObjRotPos)

drawWorld :: (Physical a n, RealFrac n, Floating n) => SDL.T.Renderer -> WorldTransform n -> World a -> IO ()
drawWorld r vt w = sequenceOf_ traverse (fmap (drawObj r vt . (view physObj)) (w ^. worldObjs))

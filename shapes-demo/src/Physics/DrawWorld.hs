module Physics.DrawWorld where

import Control.Lens hiding (transform)
import Physics.Constraint
import Physics.Draw
import Physics.Transform
import Physics.World
import qualified SDL.Video.Renderer as R

drawObj :: (RealFrac a, Floating a) => R.Renderer -> WorldTransform a -> PhysicalObj a -> IO ()
drawObj r vt o = drawConvexHull r hull
  where hull = transform vt . transform t $ (o ^. physObjHull)
        t = toTransform (o ^. physObjPos) (o ^. physObjRotPos)

drawWorld :: (Physical n a, RealFrac n, Floating n) => R.Renderer -> WorldTransform n -> World a -> IO ()
drawWorld r vt w = sequenceOf_ traverse (fmap (drawObj r vt . view physObj) (w ^. worldObjs))

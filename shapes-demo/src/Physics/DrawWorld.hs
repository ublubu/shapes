module Physics.DrawWorld where

import Control.Lens ((^.), sequenceOf_)
import Data.Array (elems)
import Physics.Constraint
import Physics.ConvexHull
import Physics.Draw
import Physics.Object
import Physics.Transform
import Physics.World
import qualified SDL.Video.Renderer as R

drawObj :: (RealFrac a, Floating a) => R.Renderer -> WorldTransform a -> ConvexHull a -> IO ()
drawObj r vt hull = drawConvexHull r (transform vt . elems . _hullVertices $ hull)

drawWorld :: (RealFrac n, Floating n) => R.Renderer -> WorldTransform n -> World (WorldObj n) -> IO ()
drawWorld r vt w = sequenceOf_ traverse (fmap (drawObj r vt . _worldShape) (w ^. worldObjs))

module Physics.Draw where

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
import Physics.Transform
import Physics.Linear
import Physics.Geometry
import Geometry

viewTransform :: (Floating a) => V2 a -> V2 a -> V2 a -> WorldTransform a
viewTransform window (V2 x y) d = joinTransforms' [ translateTransform window'
                                                  , scaleTransform (V2 x (-y))
                                                  , translateTransform (-d) ]
  where window' = fmap (/2) window

p2GeomPoint :: (RealFrac a) => P2 a -> GeomPoint
p2GeomPoint (P v) = v2GeomPoint v

v2GeomPoint :: (RealFrac a) => V2 a -> GeomPoint
v2GeomPoint (V2 x y) = toGeomPoint (x, y)

drawLine :: (RealFrac a) => SDL.T.Renderer -> P2 a -> P2 a -> IO ()
drawLine r a b = D.drawLine r (p2GeomPoint a) (p2GeomPoint b)

drawPoint :: (RealFrac a) => SDL.T.Renderer -> P2 a -> IO ()
drawPoint r p = D.drawDot r (p2GeomPoint p)

drawThickPoint :: (RealFrac a) => SDL.T.Renderer -> P2 a -> IO ()
drawThickPoint r p = D.fillRectangle r (toRect x y w h)
  where w = 4
        h = 4
        (x, y) = (p2GeomPoint p) - (2, 2)

drawConvexHull :: (RealFrac a) => SDL.T.Renderer -> ConvexHull a -> IO ()
drawConvexHull r h = sequence_ (fmap f (vList $ vertices h))
  where f v = drawLine r (vertex v) (vertex $ vNext v)

extractDepth :: (Floating a, Ord a) => LocalT a (Overlap a) -> V2 a
extractDepth = wExtract_ . lmap f
  where f ovl = fmap (*(-s)) n
          where s = overlapDepth ovl
                n = iExtract . snd . overlapEdge $ ovl

extractEdge :: (Floating a, Ord a) => LocalT a (Overlap a) -> (P2 a, P2 a)
extractEdge = wExtract_ . lmap f
  where f ovl = (g a, g b)
          where a = fst (overlapEdge ovl)
                b = lmap vNext a
                g = wExtract_ . lmap vertex

extractPenetrator :: (Floating a, Ord a) => LocalT a (Overlap a) -> P2 a
extractPenetrator = wExtract_ . lmap f
  where f = iExtract . snd . overlapPenetrator

drawOverlap :: (Floating a, RealFrac a, Ord a) => SDL.T.Renderer -> LocalT a (Overlap a) -> IO ()
drawOverlap r ovl = do
  drawLine r a b
  drawLine r c c'
  D.setColor r D.Blue
  drawThickPoint r pen
  where depth = extractDepth ovl
        (a, b) = extractEdge ovl
        c = center2 a b
        c' = c .+^ depth
        pen = extractPenetrator ovl

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
import qualified Physics.Contact as C
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

drawLine_ :: (RealFrac a) => SDL.T.Renderer -> (P2 a, P2 a) -> IO ()
drawLine_ r = uncurry (drawLine r)

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
  drawThickPoint r pen
  where depth = extractDepth ovl
        (a, b) = extractEdge ovl
        c = center2 a b
        c' = c .+^ depth
        pen = extractPenetrator ovl

extractContactPoints :: (Floating a) => LocalT a (Contact a) -> Either (P2 a) (P2 a, P2 a)
extractContactPoints cont = either (Left . wExtract_) (Right . wExtract_) $ flipEither (lmap contactPoints cont)
  where flipEither :: LocalT a (Either b c) -> Either (LocalT a b) (LocalT a c)
        flipEither (LocalT t (Left x)) = Left (LocalT t x)
        flipEither (LocalT t (Right x)) = Right (LocalT t x)

extractContactNormal :: (Floating a) => LocalT a (Contact a) -> V2 a
extractContactNormal = wExtract_ . lmap contactNormal

drawContact' :: (Floating a, RealFrac a, Show a) => SDL.T.Renderer -> LocalT a (C.Contact a) -> IO ()
drawContact' r cont = do
  drawThickPoint r p
  drawLine r p (p .+^ n)
  where p = wExtract_ . lmap C.contactPoint $ cont
        n = wExtract_ . lmap C.contactNormal $ cont

drawContact :: (Floating a, RealFrac a) => SDL.T.Renderer -> LocalT a (Contact a) -> IO ()
drawContact r cont = do
  (c, c') <- either f g ps
  drawLine r c c'
  where f a = do
          drawThickPoint r a
          return (a, a .+^ n)
        g (a, b) = do
          drawThickPoint r a
          drawThickPoint r b
          return (c, c')
            where c = center2 a b
                  c' = c .+^ n
        ps = extractContactPoints cont
        n = extractContactNormal cont

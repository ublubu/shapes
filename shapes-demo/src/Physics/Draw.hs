module Physics.Draw where

import Linear.Affine
import Linear.V2
import qualified SDL.Video.Renderer as R
import qualified Physics.Contact as C
import Physics.ConvexHull
import Physics.Transform
import Physics.Linear
import Physics.Geometry

toRenderable :: (Functor f, RealFrac a, Integral b) => f a -> f b
toRenderable = fmap floor

centeredRectangle :: (Fractional a) => P2 a -> V2 a -> R.Rectangle a
centeredRectangle center size = R.Rectangle (center .-^ halfSize) size
  where halfSize = fmap (/2) size

viewTransform :: (Floating a) => V2 a -> V2 a -> V2 a -> WorldTransform a
viewTransform window (V2 x y) d = joinTransforms' [ translateTransform window'
                                                  , scaleTransform (V2 x (-y))
                                                  , translateTransform (-d) ]
  where window' = fmap (/2) window

drawLine :: (RealFrac a) => R.Renderer -> P2 a -> P2 a -> IO ()
drawLine r a b = R.drawLine r (toRenderable a) (toRenderable b)

drawLine_ :: (RealFrac a) => R.Renderer -> (P2 a, P2 a) -> IO ()
drawLine_ r = uncurry (drawLine r)

drawPoint :: (RealFrac a) => R.Renderer -> P2 a -> IO ()
drawPoint r p = R.drawPoint r (toRenderable p)

drawThickPoint :: (RealFrac a) => R.Renderer -> P2 a -> IO ()
drawThickPoint r p = R.fillRect r (Just . toRenderable $ centeredRectangle p (V2 4 4))

drawConvexHull :: (RealFrac a) => R.Renderer -> Vertices a -> IO ()
drawConvexHull r vertices = sequence_ (fmap f segments)
  where f (v1, v2) = drawLine r v1 v2
        segments = zip vertices (tail vertices ++ [head vertices])

--extractDepth :: (Floating a, Ord a) => LocalT a (Overlap a) -> V2 a
--extractDepth = wExtract_ . lmap f
  --where f ovl = fmap (*(-s)) n
          --where s = overlapDepth ovl
                --n = iExtract . snd . overlapEdge $ ovl

--extractEdge :: (Floating a, Ord a) => LocalT a (Overlap a) -> (P2 a, P2 a)
--extractEdge = wExtract_ . lmap f
  --where f ovl = (g a, g b)
          --where a = fst (overlapEdge ovl)
                --b = lmap vNext a
                --g = wExtract_ . lmap vertex

--extractPenetrator :: (Floating a, Ord a) => LocalT a (Overlap a) -> P2 a
--extractPenetrator = wExtract_ . lmap f
  --where f = iExtract . snd . overlapPenetrator

--drawOverlap :: (Floating a, RealFrac a, Ord a) => R.Renderer -> LocalT a (Overlap a) -> IO ()
--drawOverlap r ovl = do
  --drawLine r a b
  --drawLine r c c'
  --drawThickPoint r pen
  --where depth = extractDepth ovl
        --(a, b) = extractEdge ovl
        --c = center2 a b
        --c' = c .+^ depth
        --pen = extractPenetrator ovl

--extractContactPoints :: (Floating a) => LocalT a (Contact a) -> Either (P2 a) (P2 a, P2 a)
--extractContactPoints cont = either (Left . wExtract_) (Right . wExtract_) $ flipEither (lmap contactPoints' cont)
  --where flipEither :: LocalT a (Either b c) -> Either (LocalT a b) (LocalT a c)
        --flipEither (LocalT t (Left x)) = Left (LocalT t x)
        --flipEither (LocalT t (Right x)) = Right (LocalT t x)

--extractContactNormal :: (Floating a) => LocalT a (Contact a) -> V2 a
--extractContactNormal = wExtract_ . lmap contactNormal

--drawContact' :: (Floating a, RealFrac a, Show a) => R.Renderer -> LocalT a (C.Contact a) -> IO ()
--drawContact' r cont = do
  --drawThickPoint r p
  --drawLine r p (p .+^ n)
  --where p = wExtract_ . lmap C.contactPoint $ cont
        --n = wExtract_ . lmap C.contactNormal $ cont

--drawContact :: (Floating a, RealFrac a) => R.Renderer -> LocalT a (Contact a) -> IO ()
--drawContact r cont = do
  --(c, c') <- either f g ps
  --drawLine r c c'
  --where f a = do
          --drawThickPoint r a
          --return (a, a .+^ n)
        --g (a, b) = do
          --drawThickPoint r a
          --drawThickPoint r b
          --return (c, c')
            --where c = center2 a b
                  --c' = c .+^ n
        --ps = extractContactPoints cont
        --n = extractContactNormal cont

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Physics.Draw where

import Control.Lens ((^.), _1, _2)
import Linear.Affine
import Linear.V2
import qualified SDL.Video.Renderer as R
import Physics.Draw.Canonical
import Physics.Draw.Transform (WorldTransform, joinTransforms', translateTransform, scaleTransform, translateTransform)
import Physics.Draw.Linear

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

drawAabb :: R.Renderer -> Aabb -> IO ()
drawAabb r (Aabb aabb) = do
  drawLine r nw ne
  drawLine r ne se
  drawLine r se sw
  drawLine r sw nw
  where nw = extractCorner _1 _2
        ne = extractCorner _2 _2
        sw = extractCorner _1 _1
        se = extractCorner _2 _1
        extractCorner lx ly = P $ V2 (aabb ^._x.lx) (aabb ^._y.ly)

drawPolygon :: R.Renderer -> Polygon -> IO ()
drawPolygon r vertices = sequence_ (fmap f segments)
  where f (v1, v2) = drawLine r v1 v2
        segments = zip vertices (tail vertices ++ [head vertices])

drawOverlap :: R.Renderer -> Overlap -> IO ()
drawOverlap r Overlap{..} = do
  drawLine r a b
  drawLine r c c'
  drawThickPoint r _overlapPenetrator
  where (a, b) = _overlapEdge
        c = center2 a b
        c' = c .+^ _overlapVector

drawContact :: R.Renderer -> Contact -> IO ()
drawContact r Contact{..} = do
  (c, c') <- either f g _contactPoints
  drawLine r c c'
  where f a = do
          drawThickPoint r a
          return (a, a .+^ _contactNormal)
        g (a, b) = do
          drawThickPoint r a
          drawThickPoint r b
          return (c, c')
            where c = center2 a b
                  c' = c .+^ _contactNormal

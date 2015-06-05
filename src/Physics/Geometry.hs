{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Physics.Geometry where

import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.List.Zipper
import Linear.Epsilon
import Linear.Matrix
import Linear.Metric
import Linear.V2
import Linear.Vector
import Utils.Utils
import Physics.Linear
import Physics.Transform

data ConvexHull a = ConvexHull { hullVertices :: [V2 a] }
data VertexView a = VertexView Int (Loop (V2 a))

rectangleHull :: (Fractional a) => a -> a -> ConvexHull a
rectangleHull w h = ConvexHull [ V2 w2 h2
                               , V2 (-w2) h2
                               , V2 (-w2) (-h2)
                               , V2 w2 (-h2) ]
  where w2 = w / 2
        h2 = h / 2

vertices :: ConvexHull a -> VertexView a
vertices (ConvexHull vs) = VertexView (length vs) (loopify vs)

vPrev :: VertexView a -> VertexView a
vPrev (VertexView n vs) = VertexView n (loopPrev vs)

vNext :: VertexView a -> VertexView a
vNext (VertexView n vs) = VertexView n (loopNext vs)

vCount :: VertexView a -> Int
vCount (VertexView n _) = n

vList :: VertexView a -> [VertexView a]
vList v = take (vCount v) (iterate vNext v)

vertexLoop :: VertexView a -> Loop (V2 a)
vertexLoop (VertexView _ l) = l

vertex :: VertexView a -> V2 a
vertex = loopVal . vertexLoop

edgeNormal :: (Num a, Ord a) => VertexView a -> V2 a
edgeNormal vs = clockwise2 (v' - v)
  where v = vertex vs
        v' = vertex (vNext vs)

unitEdgeNormal :: (Epsilon a, Floating a, Ord a) => VertexView a -> V2 a
unitEdgeNormal = normalize . edgeNormal

support :: (Num a, Ord a) => VertexView a -> V2 a -> VertexView a
support v dir = snd $ foldl1 g (fmap f vs)
  where vs = vList v
        f v' = let point = vertex v' in (dir `dot` point, v')
        g a@(distA, _) b@(distB, _) = if distB > distA then b else a

{-

extentAlong :: (Num a) => Support a -> V2 a -> (V2 a, V2 a)
extentAlong sa dir = (sa (-dir), sa dir)

data Overlap a = Overlap { overlapDir :: V2 a
                         , overlapEdge :: Edge a ()
                         , overlapFeature :: (Edge a (), Edge a ())}

--overlapAlong :: (Num a) => Support a -> V2 a -> Support a -> Overlap a
--overlapAlong sa dir sb =
  --where xa = sa `extentAlong` dir
        --xb = sb `extentAlong` dir
-}

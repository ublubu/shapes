{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Physics.Geometry where

import Control.Monad
import Control.Applicative
import qualified Control.Lens as L
import Data.Maybe
import Data.List.Zipper
import Linear.Affine
import Linear.Epsilon
import Linear.Matrix
import Linear.Metric
import Linear.V2
import Linear.Vector
import Utils.Utils
import Physics.Linear
import Physics.Transform

data ConvexHull a = ConvexHull { hullVertices :: [P2 a] }
data VertexView a = VertexView Int (Loop (P2 a))

rectangleHull :: (Fractional a) => a -> a -> ConvexHull a
rectangleHull w h = ConvexHull [ P $ V2 w2 h2
                               , P $ V2 (-w2) h2
                               , P $ V2 (-w2) (-h2)
                               , P $ V2 w2 (-h2) ]
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

vertexLoop :: VertexView a -> Loop (P2 a)
vertexLoop (VertexView _ l) = l

vertex :: VertexView a -> P2 a
vertex = loopVal . vertexLoop

edgeNormal :: (Num a, Ord a) => VertexView a -> V2 a
edgeNormal vs = clockwise2 (v' .-. v)
  where v = vertex vs
        v' = vertex (vNext vs)

unitEdgeNormal :: (Epsilon a, Floating a, Ord a) => VertexView a -> V2 a
unitEdgeNormal = normalize . edgeNormal

support :: (Num a, Ord a) => VertexView a -> V2 a -> VertexView a
support v dir = snd $ foldl1 g (fmap f vs)
  where vs = vList v
        f v' = let point = vertex v' in (dir `afdot'` point, v')
        g a@(distA, _) b@(distB, _) = if distB > distA then b else a

type Feature a b = (LocalT a (VertexView a), b)
type Support a = WV2 a -> Feature a (WP2 a)

support' :: (Floating a, Ord a) => LocalT a (VertexView a) -> Support a
support' v dir = (v', wExtract (lmap vertex v'))
  where sup = lmap support v
        v' = lwap sup dir

extentAlong :: (Floating a, Ord a) => Support a -> WV2 a -> (Feature a (WP2 a), Feature a (WP2 a))
extentAlong sup dir = (minv, maxv)
  where minv = sup (wmap negate dir)
        maxv = sup dir

-- assumes pairs are (min, max)
overlapTest :: (Ord a) => (a, a) -> (a, a) -> Bool
overlapTest (a, b) (c, d) = not (c > b || d < a)

-- intervals are of distance along edge normal of shape X
overlapAmount :: (Ord a, Num a) => (a, a) -> (a, a) -> Maybe a
overlapAmount x@(_, edge) y@(penetrator, _) = toMaybe (overlapTest x y) (edge - penetrator)

unitEdgeNormals :: (Epsilon a, Floating a, Ord a) => LocalT a (VertexView a) -> [Feature a (WV2 a)]
unitEdgeNormals v = fmap f vs
  where f v' = (v', wExtract (lmap unitEdgeNormal v'))
        vs = lfmap vList v

data Overlap a = Overlap { overlapEdge :: Feature a (WV2 a)
                         , overlapDepth :: a
                         , overlapPenetrator :: Feature a (WP2 a) }

overlap :: (Floating a, Ord a) => Support a -> WV2 a -> Support a -> Maybe (Overlap a)
overlap ss dir sp = fmap (\oval' -> Overlap { overlapEdge = L.set L._2 dir edge
                                             , overlapDepth = oval'
                                             , overlapPenetrator = penetrator }) oval
  where extentS@(_, edge) = extentAlong ss dir
        extentP@(penetrator, _) = extentAlong sp dir
        projectedExtent ex = pairMap f (pairMap snd ex)
                            where f v = iExtract (wap (wmap afdot' dir) v)
        oval = overlapAmount (projectedExtent extentS) (projectedExtent extentP)

greatestOverlap :: (Floating a, Ord a) => Support a -> [WV2 a] -> Support a -> Maybe (Overlap a)
greatestOverlap ss dirs sp = foldl1 f os
  where os = fmap (\dir -> overlap ss dir sp) dirs
        f maxo o = do
          maxo' <- maxo
          o' <- o
          return (if overlapDepth o' > overlapDepth maxo' then o' else maxo')

--clipEdge :: (P2 a, P2 a) -> (P2 a, P2 a)

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

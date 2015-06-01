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

type Support a = V2 a -> VertexView a

support :: (Num a, Ord a) => VertexView a -> Support a
support v dir = snd $ foldl1 g (fmap f vs)
  where vs = vList v
        f v' = let point = vertex v' in (dir `dot` point, v')
        g a@(distA, _) b@(distB, _) = if distB > distA then b else a

minkowskiSupport :: (Floating a, Ord a) => WorldT' (VertexView a) a -> WorldT' (VertexView a) a -> Support a
minkowskiSupport  sa sb =


{-support_ :: (Floating a, Ord a) => ConvexHull a -> V2 a -> V2 a
support_ shape dir = furthestAlong dir (hullVertices shape)

support :: (Floating a, Ord a) => WorldT (ConvexHull a) -> V2 a -> Zipper (WorldT (V2 a))
support = outerTrans support_

flipSupport :: (Floating a, Ord a) => (V2 a -> V2 a) -> V2 a -> V2 a
flipSupport f dir = (-(f (-dir)))

minkowskiSupport :: (Floating a, Ord a) => WorldT (ConvexHull a) -> WorldT (ConvexHull a) -> V2 a -> V2 a
minkowskiSupport a b dir = support a dir + flipSupport (support b) dir

data Simplex a = Simplex1 (V2 a) | Simplex2 (V2 a) (V2 a) | Simplex3 (V2 a) (V2 a) (V2 a)

containOrigin :: (Num a, Ord a) => (V2 a -> V2 a) -> Simplex a -> Maybe (Simplex a)
containOrigin f (Simplex1 a) = toMaybe passedOrigin =<< containOrigin f (Simplex2 a' a)
  where dir = (-a)
        a' = f dir
        passedOrigin = not (similarDir a' a)
containOrigin f (Simplex2 a b) = if similarDir ab a
                                 then containOrigin f (Simplex1 a)
                                 else containOrigin f (Simplex3 (f dir) a b)
  where ab = b - a
        a0 = (-a)
        dir = ab `perpendicularTowards` a0
containOrigin f s@(Simplex3 a b c) =
  if similarDir acOut a0
  then if similarDir ac a0 then containOrigin f (Simplex2 a c)
       else star
  else if similarDir abOut a0 then star
       else Just s
  where acOut = ac `perpendicularTowards` ba
        ac = c - a
        ba = a - b
        abOut = ab `perpendicularTowards` ca
        ab = (-ba)
        ca = (-ac)
        a0 = (-a)
        star = if similarDir ab a0 then containOrigin f (Simplex2 a b)
               else containOrigin f (Simplex1 a)

unitX2 :: Num a => V2 a
unitX2 = V2 1 0

overlaps :: (Floating a, Ord a) => WorldT (ConvexHull a) -> WorldT (ConvexHull a) -> Bool
a `overlaps` b = isJust (containOrigin ms (Simplex1 $ ms unitX2))
  where ms = minkowskiSupport a b

--axisOverlaps :: (Epsilon a, Floating a, Ord a) => WorldT (ConvexHull a) -> WorldT (ConvexHull a) -> Maybe [(V2 a, a)]
--axisOverlaps a b = (++) <$> axisOverlaps_ a sb <*> axisOverlaps_ b sa
  --where sa = support a
        --sb = support b

--axisOverlaps_ :: (Epsilon a, Floating a, Ord a) => WorldT (ConvexHull a) -> (V2 a -> V2 a) -> Maybe [(V2 a, a)]
--axisOverlaps_ hull sup = takeIfAll (\(_, d) -> d > 0) (fmap f edges)
  --where edges = zip (postTrans' hullVertices hull) (postTrans' unitEdgeNormals hull)
        --f (v, n) = (v', (v' - v) `dot` n')
          --where n' = (-n)
                --v' = sup n'

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

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Physics.ConvexHull where

import qualified Control.Lens as L
import Data.Array
import Linear.Affine
import Linear.Epsilon
import Linear.Metric
import Linear.V2
import Utils.Utils
import Physics.Linear
import Physics.Transform
import Physics.Geometry (Neighborhood(..), HasSupport(..), HasNeighborhoods(..))

type Vertices a = [P2 a]

data ConvexHull a =
  ConvexHull { _hullVertexCount :: !Int
             , _hullVertices :: !(Array Int (P2 a))
             , _hullEdgeNormals :: !(Array Int (V2 a))
             --, _hullExtents :: !(Array Int (a, a))
             , _hullNeighborhoods :: Array Int (Neighborhood a)
             } deriving (Show, Eq)

instance (Epsilon a, Floating a, Ord a) => HasNeighborhoods ConvexHull a where
  neighborhoods = elems . _hullNeighborhoods

instance (Epsilon a, Floating a, Ord a) => HasSupport ConvexHull a where
  support ConvexHull{..} dir = snd . foldl1 g $ fmap f _hullNeighborhoods
    where f neigh@Neighborhood{..} = (dir `afdot'` _neighborhoodCenter, neigh)
          g a@(distA, _) b@(distB, _) = if distB > distA then b else a
  extentAlong ConvexHull{..} dir = pairMap snd . foldl1 g $ fmap f _hullNeighborhoods
    where f neigh@Neighborhood{..} =
            ((dist, neigh), (dist, neigh))
            where dist = dir `afdot'` _neighborhoodCenter
          g (minA@(minDistA, _), maxA@(maxDistA, _)) (minB@(minDistB, _), maxB@(maxDistB, _)) =
            (minAB, maxAB)
            where minAB = if minDistB < minDistA then minB else minA
                  maxAB = if maxDistB > maxDistA then maxB else maxA


instance (Epsilon a, Floating a, Ord a) => WorldTransformable (ConvexHull a) a where
  transform t =
    listToHull . fmap (transform t) . elems . _hullVertices
  untransform t =
    listToHull . fmap (untransform t) . elems . _hullVertices

rectangleVertices :: (Fractional a) => a -> a -> Vertices a
rectangleVertices w h =
  [ P $ V2 w2 h2
  , P $ V2 (-w2) h2
  , P $ V2 (-w2) (-h2)
  , P $ V2 w2 (-h2) ]
  where w2 = w / 2
        h2 = h / 2

rectangleHull :: (Epsilon a, Floating a, Ord a) => a -> a -> ConvexHull a
rectangleHull w h = listToHull $ rectangleVertices w h

listToHull :: (Epsilon a, Floating a, Ord a) => [P2 a] -> ConvexHull a
listToHull vertices =
  hull
  where vertexCount = length vertices
        vertexBound = vertexCount - 1
        vertexBounds = (0, vertexBound)
        vertices' = listArray vertexBounds vertices
        edgeNormals = ixedMap (unitEdgeNormal vertexBound) vertices'
        --extents = 
        hull = ConvexHull vertexCount
               vertices'
               edgeNormals
               --extents
               (makeNeighborhoods hull)

makeNeighborhoods :: ConvexHull a -> Array Int (Neighborhood a)
makeNeighborhoods hull@ConvexHull{..} =
  listArray (bounds _hullVertices) $
  fmap (makeNeighborhood hull) (indices _hullVertices)

makeNeighborhood :: ConvexHull a -> Int -> Neighborhood a
makeNeighborhood ConvexHull{..} i =
  Neighborhood { _neighborhoodCenter = (_hullVertices ! i)
               , _neighborhoodNext = (_hullNeighborhoods ! (nextIndex maxIndex i))
               , _neighborhoodPrev = (_hullNeighborhoods ! (prevIndex maxIndex i))
               , _neighborhoodUnitNormal = (_hullEdgeNormals ! i)
               , _neighborhoodIndex = i
               }
  where maxIndex = arrMaxBound _hullVertices

ixedMap :: (Ix i) => (Array i e -> i -> x) -> Array i e -> Array i x
ixedMap f arr = listArray (bounds arr) $ fmap (f arr) (indices arr)

edgeNormal :: (Num a, Ord a) => Int -> Array Int (P2 a) -> Int -> V2 a
edgeNormal maxIndex vs i = clockwise2 (v' .-. v)
  where v = vs ! i
        v' = vs ! (nextIndex maxIndex i)

--extent :: (Num a, Ord a) => Int -> V2 a -> 

unitEdgeNormal :: (Epsilon a, Floating a, Ord a) => Int -> Array Int (P2 a) -> Int -> V2 a
unitEdgeNormal maxIndex vs = normalize . edgeNormal maxIndex vs

arrMaxBound :: Array Int a -> Int
arrMaxBound = snd . bounds

nextIndex :: Int -> Int -> Int
nextIndex max i = if i < max then i + 1 else 0

prevIndex :: Int -> Int -> Int
prevIndex max i = if i > 0 then i - 1 else max

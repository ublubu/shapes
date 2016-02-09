{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Physics.ConvexHull where

import Control.Lens ((^.), (.~), (%~), makeLenses)
import Data.Array
import Linear.Affine
import Linear.Epsilon
import Linear.Metric
import Linear.V2
import Utils.Utils
import Physics.Linear
import Physics.Transform

data Neighborhood a = Neighborhood { _neighborhoodCenter :: !(P2 a)
                                   , _neighborhoodNext :: Neighborhood a
                                   , _neighborhoodPrev :: Neighborhood a
                                   , _neighborhoodUnitNormal :: !(V2 a)
                                   , _neighborhoodIndex :: !Int
                                   } deriving (Eq)
makeLenses ''Neighborhood

data Extent a f =
  Extent { _extentMin :: !f
         , _extentMax :: !f
         , _extentProjection :: !(a, a)
         } deriving (Show, Eq)
makeLenses ''Extent

instance Functor (Extent a) where
  fmap f (Extent x y p) = Extent (f x) (f y) p

instance (Show a) => Show (Neighborhood a) where
  show Neighborhood{..} =
    "Neighborhood (" ++
    show _neighborhoodCenter ++ ") (" ++
    show _neighborhoodUnitNormal ++ ") (" ++
    show _neighborhoodIndex ++ ")"

type Vertices a = [P2 a]

data ConvexHull a =
  ConvexHull { _hullVertexCount :: !Int
             , _hullVertices :: !(Array Int (P2 a))
             , _hullEdgeNormals :: !(Array Int (V2 a))
             , _hullNeighborhoods :: Array Int (Neighborhood a)
             , _hullExtents :: !(Array Int (Int, Int))
             , _hullLocalVertices :: !(Array Int (P2 a))
             } deriving (Show, Eq)
makeLenses ''ConvexHull

--instance WorldTransformable a (CHWorldCache a) where
  --transform t CHWorldCache{..} =

distanceAlong :: (Num a) => Neighborhood a -> V2 a -> a
Neighborhood{..} `distanceAlong` dir =
  dir `afdot'` _neighborhoodCenter

extentAlong' :: (Num a, Ord a) => ConvexHull a -> V2 a -> (Neighborhood a, Neighborhood a)
extentAlong' ConvexHull{..} dir = pairMap snd . foldl1 g $ fmap f _hullNeighborhoods
  where f neigh =
          ((dist, neigh), (dist, neigh))
          where dist = neigh `distanceAlong` dir
        g (minA@(minDistA, _), maxA@(maxDistA, _)) (minB@(minDistB, _), maxB@(maxDistB, _)) =
          (minAB, maxAB)
          where minAB = if minDistB < minDistA then minB else minA
                maxAB = if maxDistB > maxDistA then maxB else maxA

extentAlong :: (Num a, Ord a) => ConvexHull a -> V2 a -> Extent a (Neighborhood a)
extentAlong shape dir =
  Extent minv maxv projectedExtent
  where projectedExtent = pairMap f (pairMap _neighborhoodCenter ext)
          where f v = dir `afdot'` v
        ext@(minv, maxv) = extentAlong' shape dir

extentIndices :: Extent a (Neighborhood a) -> (Int, Int)
extentIndices ext =
  (_neighborhoodIndex . _extentMin $ ext, _neighborhoodIndex . _extentMax $ ext)

extentAlongSelf' :: ConvexHull a -> Int -> (Int, Int)
extentAlongSelf' ConvexHull{..} index = _hullExtents ! index

extentAlongSelf :: (Num a) => ConvexHull a -> (Int, V2 a) -> Extent a (Neighborhood a)
extentAlongSelf hull@ConvexHull{..} (index, dir) =
  Extent { _extentMin = minN
         , _extentMax = maxN
         , _extentProjection = (minN `distanceAlong` dir, maxN `distanceAlong` dir)}
  where (minN, maxN) = pairMap (_hullNeighborhoods !) $ extentAlongSelf' hull index

neighborhoods :: ConvexHull a -> [Neighborhood a]
neighborhoods = elems . _hullNeighborhoods

support :: (Num a, Ord a) => ConvexHull a -> V2 a -> Neighborhood a
support ConvexHull{..} dir = snd . foldl1 g $ fmap f _hullNeighborhoods
  where f neigh@Neighborhood{..} = (dir `afdot'` _neighborhoodCenter, neigh)
        g a@(distA, _) b@(distB, _) = if distB > distA then b else a

-- TODO: make ConvexHull a proper WorldTransformable
--instance (Epsilon a, Floating a, Ord a) => WorldTransformable (ConvexHull a) a where
  --transform t = flip transformHull (transform t)
  --untransform t = flip transformHull (untransform t)

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

listToHull :: forall a . (Epsilon a, Floating a, Ord a) => [P2 a] -> ConvexHull a
listToHull vertices =
  hull
  where vertexCount = length vertices
        vertexBound = vertexCount - 1
        vertexBounds = (0, vertexBound)
        vertices' = listArray vertexBounds vertices
        edgeNormals = ixedMap (unitEdgeNormal vertexBound) vertices'
        extents = fmap (extentIndices . extentAlong hull) edgeNormals
        hull :: ConvexHull a
        hull = ConvexHull vertexCount
               vertices'
               edgeNormals
               (makeNeighborhoods hull)
               extents
               vertices'

-- assumes scale-invariant transform in worldspace
transformHull :: (Epsilon a, Floating a, Ord a)
              => ConvexHull a
              -> (P2 a -> P2 a)
              -> ConvexHull a
transformHull hull@ConvexHull{..} fInWorldSpace =
  hull'
  where hull' = hull { _hullVertices = vertices
                     , _hullEdgeNormals = edgeNormals
                     , _hullNeighborhoods = makeNeighborhoods hull
                     }
        vertices = fmap fInWorldSpace _hullVertices
        edgeNormals = ixedMap (unitEdgeNormal $ _hullVertexCount - 1) vertices

-- assumes scale-invariant transform from localspace
setHullTransform :: (Epsilon a, Floating a, Ord a)
                 => ConvexHull a
                 -> (P2 a -> P2 a)
                 -> ConvexHull a
setHullTransform hull@ConvexHull{..} fromLocalSpace =
  hull'
  where hull' = hull { _hullVertices = vertices
                     , _hullEdgeNormals = edgeNormals
                     , _hullNeighborhoods = makeNeighborhoods hull'
                     }
        vertices = fmap fromLocalSpace _hullLocalVertices
        edgeNormals = ixedMap (unitEdgeNormal $ _hullVertexCount - 1) vertices

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

unitEdgeNormal :: (Epsilon a, Floating a, Ord a) => Int -> Array Int (P2 a) -> Int -> V2 a
unitEdgeNormal maxIndex vs = normalize . edgeNormal maxIndex vs

arrMaxBound :: Array Int a -> Int
arrMaxBound = snd . bounds

nextIndex :: Int -> Int -> Int
nextIndex max i = if i < max then i + 1 else 0

prevIndex :: Int -> Int -> Int
prevIndex max i = if i > 0 then i - 1 else max

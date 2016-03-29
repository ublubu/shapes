{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Physics.Contact.Opt.ConvexHull where

import GHC.Generics (Generic)
import GHC.Prim (Double#, (/##), negateDouble#)

import Control.DeepSeq
import Control.Lens (makeLenses)
import Data.Array
import Physics.Linear.Opt
import Utils.Utils

data Neighborhood = Neighborhood { _neighborhoodCenter :: !P2
                                 , _neighborhoodNext :: Neighborhood
                                 , _neighborhoodPrev :: Neighborhood
                                 , _neighborhoodUnitNormal :: !V2
                                 , _neighborhoodIndex :: !Int
                                 } deriving (Generic)
makeLenses ''Neighborhood

instance NFData Neighborhood where
  rnf (Neighborhood a _ _ b c) = rnf (a, b, c)

data Extent f =
  Extent { _extentMin :: !f
         , _extentMax :: !f
         , _extentProjection :: !(SP Double Double)
         } deriving (Show, Eq, Generic, NFData)
makeLenses ''Extent

instance Functor Extent where
  fmap f (Extent x y p) = Extent (f x) (f y) p

instance Show Neighborhood where
  show Neighborhood{..} =
    "Neighborhood (" ++
    show _neighborhoodCenter ++ ") (" ++
    show _neighborhoodUnitNormal ++ ") (" ++
    show _neighborhoodIndex ++ ")"

type Vertices = [P2]

data ConvexHull =
  ConvexHull { _hullVertexCount :: !Int
             , _hullVertices :: !(Array Int P2)
             , _hullEdgeNormals :: !(Array Int V2)
             , _hullNeighborhoods :: Array Int Neighborhood
             , _hullExtents :: !(Array Int (Int, Int))
             , _hullLocalVertices :: !(Array Int P2)
             } deriving (Show, Generic, NFData)
makeLenses ''ConvexHull

_hullNeighborhood :: Int -> ConvexHull -> Neighborhood
_hullNeighborhood i hull =
  _hullNeighborhoods hull ! i

distanceAlong :: Neighborhood -> V2 -> Double
Neighborhood{..} `distanceAlong` dir =
  dir `afdot'` _neighborhoodCenter

extentAlong' :: ConvexHull -> V2 -> SP Neighborhood Neighborhood
extentAlong' ConvexHull{..} dir = toSP . pairMap snd . foldl1 g $ fmap f _hullNeighborhoods
  where f neigh =
          ((dist, neigh), (dist, neigh))
          where dist = neigh `distanceAlong` dir
        g (minA@(minDistA, _), maxA@(maxDistA, _)) (minB@(minDistB, _), maxB@(maxDistB, _)) =
          (minAB, maxAB)
          where minAB = if minDistB < minDistA then minB else minA
                maxAB = if maxDistB > maxDistA then maxB else maxA

extentAlong :: ConvexHull -> V2 -> Extent Neighborhood
extentAlong shape dir =
  Extent minv maxv projectedExtent
  where projectedExtent = spMap (f . _neighborhoodCenter) ext
          where f v = dir `afdot'` v
        ext@(SP minv maxv) = extentAlong' shape dir

extentIndices :: Extent Neighborhood -> (Int, Int)
extentIndices ext =
  (_neighborhoodIndex . _extentMin $ ext, _neighborhoodIndex . _extentMax $ ext)

extentAlongSelf' :: ConvexHull -> Int -> (Int, Int)
extentAlongSelf' ConvexHull{..} = (_hullExtents !)

extentAlongSelf :: ConvexHull -> (Int, V2) -> Extent Neighborhood
extentAlongSelf hull@ConvexHull{..} (index', dir) =
  Extent { _extentMin = minN
         , _extentMax = maxN
         , _extentProjection = SP (minN `distanceAlong` dir) (maxN `distanceAlong` dir)
         }
  where (minN, maxN) = pairMap (_hullNeighborhoods !) $ extentAlongSelf' hull index'

neighborhoods :: ConvexHull -> [Neighborhood]
neighborhoods = elems . _hullNeighborhoods

support :: ConvexHull -> V2 -> Neighborhood
support ConvexHull{..} dir = snd . foldl1 g $ fmap f _hullNeighborhoods
  where f neigh@Neighborhood{..} = (dir `afdot'` _neighborhoodCenter, neigh)
        g a@(distA, _) b@(distB, _) = if distB > distA then b else a

-- TODO: make ConvexHull a proper WorldTransformable
--instance (Epsilon a, Floating a, Ord a) => WorldTransformable (ConvexHull a) a where
  --transform t = flip transformHull (transform t)
  --untransform t = flip transformHull (untransform t)

rectangleVertices :: Double# -> Double# -> Vertices
rectangleVertices w h =
  [ P2 $ V2 w2 h2
  , P2 $ V2 nw2 h2
  , P2 $ V2 nw2 nh2
  , P2 $ V2 w2 nh2 ]
  where w2 = w /## 2.0##
        h2 = h /## 2.0##
        nw2 = negateDouble# w2
        nh2 = negateDouble# h2

rectangleHull :: Double# -> Double# -> ConvexHull
rectangleHull w h = listToHull $ rectangleVertices w h

listToHull :: [P2] -> ConvexHull
listToHull vertices =
  hull
  where vertexCount = length vertices
        vertexBound = vertexCount - 1
        vertexBounds = (0, vertexBound)
        vertices' = listArray vertexBounds vertices
        edgeNormals = ixedMap (unitEdgeNormal vertexBound) vertices'
        extents = fmap (extentIndices . extentAlong hull) edgeNormals
        hull :: ConvexHull
        hull = ConvexHull vertexCount
               vertices'
               edgeNormals
               (makeNeighborhoods hull)
               extents
               vertices'

-- assumes scale-invariant transform in worldspace
transformHull ::  ConvexHull
              -> (P2 -> P2)
              -> ConvexHull
transformHull hull@ConvexHull{..} fInWorldSpace =
  hull'
  where hull' = hull { _hullVertices = vertices
                     , _hullEdgeNormals = edgeNormals
                     , _hullNeighborhoods = makeNeighborhoods hull
                     }
        vertices = fmap fInWorldSpace _hullVertices
        edgeNormals = ixedMap (unitEdgeNormal $ _hullVertexCount - 1) vertices

-- assumes scale-invariant transform from localspace
setHullTransform :: ConvexHull
                 -> (P2 -> P2)
                 -> ConvexHull
setHullTransform hull@ConvexHull{..} fromLocalSpace =
  hull'
  where hull' = hull { _hullVertices = vertices
                     , _hullEdgeNormals = edgeNormals
                     , _hullNeighborhoods = makeNeighborhoods hull'
                     }
        vertices = fmap fromLocalSpace _hullLocalVertices
        edgeNormals = ixedMap (unitEdgeNormal $ _hullVertexCount - 1) vertices

makeNeighborhoods :: ConvexHull -> Array Int Neighborhood
makeNeighborhoods hull@ConvexHull{..} =
  listArray (bounds _hullVertices) $
  fmap (makeNeighborhood hull) (indices _hullVertices)

makeNeighborhood :: ConvexHull -> Int -> Neighborhood
makeNeighborhood ConvexHull{..} i =
  Neighborhood { _neighborhoodCenter = _hullVertices ! i
               , _neighborhoodNext = _hullNeighborhoods ! nextIndex maxIndex i
               , _neighborhoodPrev = _hullNeighborhoods ! prevIndex maxIndex i
               , _neighborhoodUnitNormal = _hullEdgeNormals ! i
               , _neighborhoodIndex = i
               }
  where maxIndex = arrMaxBound _hullVertices

ixedMap :: (Ix i) => (Array i e -> i -> x) -> Array i e -> Array i x
ixedMap f arr = listArray (bounds arr) $ fmap (f arr) (indices arr)

edgeNormal :: Int -> Array Int P2 -> Int -> V2
edgeNormal maxIndex vs i = clockwiseV2 (v' `diffP2` v)
  where v = vs ! i
        v' = vs ! nextIndex maxIndex i

unitEdgeNormal :: Int -> Array Int P2 -> Int -> V2
unitEdgeNormal maxIndex vs = normalizeV2 . edgeNormal maxIndex vs

arrMaxBound :: Array Int a -> Int
arrMaxBound = snd . bounds

nextIndex :: Int -> Int -> Int
nextIndex max_i i = if i < max_i then i + 1 else 0

prevIndex :: Int -> Int -> Int
prevIndex max_i i = if i > 0 then i - 1 else max_i

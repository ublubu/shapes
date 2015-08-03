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
import Data.Either
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

data ConvexHull a = ConvexHull { hullVertices :: [P2 a] } deriving Show
data VertexView a = VertexView Int (Loop (P2 a)) deriving Show

instance (Floating a) => WorldTransformable (ConvexHull a) a where
  transform t (ConvexHull vs) = ConvexHull (fmap (transform t) vs)
  untransform t (ConvexHull vs) = ConvexHull (fmap (untransform t) vs)

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

type ShapeInfo a = (Support a, [Feature a (WV2 a)])

shapeInfo :: (Epsilon a, Floating a, Ord a) => LocalT a (ConvexHull a) -> ShapeInfo a
shapeInfo h = (sup, edges)
  where sup = support' vs
        vs = lmap vertices h
        edges = unitEdgeNormals vs

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

data Overlap a = Overlap { overlapEdge :: Feature a (WV2 a) -- unit normal
                         , overlapDepth :: a
                         , overlapPenetrator :: Feature a (WP2 a) } -- vertex in world coords
                 deriving Show

overlapNormal :: Overlap a -> WV2 a
overlapNormal = snd . overlapEdge

overlap :: (Floating a, Ord a) => Support a -> Feature a (WV2 a) -> Support a -> Maybe (Overlap a)
overlap ss edge@(_, dir) sp = fmap (\oval' -> Overlap { overlapEdge = edge
                                             , overlapDepth = oval'
                                             , overlapPenetrator = penetrator }) oval
  where extentS = extentAlong ss dir
        extentP@(penetrator, _) = extentAlong sp dir
        projectedExtent ex = pairMap f (pairMap snd ex)
                            where f v = iExtract (wap (wmap afdot' dir) v)
        oval = overlapAmount (projectedExtent extentS) (projectedExtent extentP)

minOverlap :: (Floating a, Ord a) => Support a -> [Feature a (WV2 a)] -> Support a -> Maybe (Overlap a)
minOverlap ss edges sp = foldl1 f os
  where os = fmap (\edge -> overlap ss edge sp) edges
        f mino o = do
          mino' <- mino
          o' <- o
          return (if overlapDepth o' < overlapDepth mino' then o' else mino')

minOverlap' :: (Floating a, Ord a) => ShapeInfo a -> ShapeInfo a -> Maybe (Overlap a)
minOverlap' (sa, esa) (sb, _) = minOverlap sa esa sb

contactDepth :: (Floating a) => Feature a (WV2 a) -> WP2 a -> a
contactDepth (v, n) p = f v' - f p
  where v' = wExtract (lmap vertex v)
        f = wlift2_ afdot' n

penetratingEdge :: (Floating a, Ord a) => Overlap a -> WorldT (P2 a, P2 a)
penetratingEdge (Overlap (ve, n) depth (vp, b)) = if wlift2_ (<) bcn abn then wlift2 (,) b c
                                                  else wlift2 (,) a b
  where c = wExtract . lmap (vertex . vNext) $ vp
        a = wExtract . lmap (vertex . vPrev) $ vp
        abn = wmap abs $ wlift2 dot (wlift2 (.-.) b a) n
        bcn = wmap abs $ wlift2 dot (wlift2 (.-.) c b) n

penetratedEdge :: (Floating a) => Overlap a -> WorldT (P2 a, P2 a)
penetratedEdge (Overlap (ve, _) _ _) = wlift2 (,) a b
  where a = wExtract . lmap vertex $ ve
        b = wExtract . lmap (vertex . vNext) $ ve

type ContactPoint a = (P2 a, a)
data Contact a = Contact { contactPoints :: Either (P2 a) (P2 a, P2 a)
                         , contactNormal :: V2 a } deriving Show

flattenContactPoints :: Contact a -> [P2 a]
flattenContactPoints (Contact (Left p) _) = [p]
flattenContactPoints (Contact (Right (p1, p2)) _) = [p1, p2]

clipEdge :: (Floating a, Epsilon a, Ord a) => (P2 a, P2 a) -> V2 a -> (P2 a, P2 a) -> Maybe (Contact a)
clipEdge (a, b) n inc@(c, d) = do
  inc' <- applyClip' (clipSegment aBound (cd, inc)) inc
  inc'' <- applyClip' (clipSegment bBound (cd, inc')) inc'
  contacts <- applyClip'' (clipSegment abBound (cd, inc'')) inc''
  return Contact { contactPoints = contacts
                  , contactNormal = n }
  where aBound = perpLine2 a b
        bBound = perpLine2 b a
        abBound = Line2 a (-n)
        cd = toLine2 c d

-- 'Flipping' indicates the direction of the collision. 'Same' means the first object overlaps into the second.
contact :: (Floating a, Epsilon a, Ord a) => ShapeInfo a -> ShapeInfo a -> Maybe (Flipping (WorldT (Contact a), Feature a (WV2 a)))
contact a b = either (fmap Same . contact_) (fmap Flip . contact_) =<< ovl
  where ovlab = minOverlap' a b
        ovlba = minOverlap' b a
        ovl = maybeBranch (\oab oba -> overlapDepth oab < overlapDepth oba) ovlab ovlba

contact_ :: (Floating a, Epsilon a, Ord a) => Overlap a -> Maybe (WorldT (Contact a), Feature a (WV2 a))
contact_ ovl = fmap f (wflip $ (wmap clipEdge edge) `wap` n `wap` pen)
  where edge = penetratedEdge ovl
        pen = penetratingEdge ovl
        n = overlapNormal ovl
        f c = (c, overlapEdge ovl)

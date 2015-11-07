{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Physics.BenchGeometry where

import qualified Control.Lens as L
import Data.Either.Combinators
import Linear.Affine
import Linear.Epsilon
import Linear.Metric
import Linear.V2
import Utils.Utils
import Physics.Linear
import Physics.Transform

data ConvexHull a = ConvexHull { hullVertices :: ![P2 a] } deriving Show
data VertexView a = VertexView !Int !(Loop (P2 a)) !Int deriving Show

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
vertices (ConvexHull vs) = VertexView (length vs) (loopify vs) 0

vPrev :: VertexView a -> VertexView a
vPrev (VertexView n vs i) = VertexView n (loopPrev vs) (i - 1 `posMod` n)

vNext :: VertexView a -> VertexView a
vNext (VertexView n vs i) = VertexView n (loopNext vs) (i + 1 `posMod` n)

vCount :: VertexView a -> Int
vCount (VertexView n _ _) = n

vList :: VertexView a -> [VertexView a]
vList v = take (vCount v) (iterate vNext v)

vIndex :: VertexView a -> Int
vIndex (VertexView _ _ i) = i

vertexLoop :: VertexView a -> Loop (P2 a)
vertexLoop (VertexView _ l _) = l

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

type Feature a b = (VertexView a, b)
type Support a = V2 a -> Feature a (P2 a)

type ShapeInfo a = (Support a, [Feature a (V2 a)])

data Overlap a = Overlap { overlapEdge :: !(Feature a (V2 a)) -- unit normal
                         , overlapDepth :: !a
                         , overlapPenetrator :: !(ContactP2 a) } -- vertex in world coords
                 deriving Show

shapeInfo :: (Epsilon a, Floating a, Ord a) => ConvexHull a -> ShapeInfo a
shapeInfo h = (sup, edges)
  where sup = support' vs
        vs = vertices h
        edges = unitEdgeNormals vs

support' :: (Floating a, Ord a) => VertexView a -> Support a
support' v dir = (v', vertex v')
  where sup = support v
        v' = sup dir

extentAlong :: (Floating a, Ord a) => Support a -> V2 a -> (Feature a (P2 a), Feature a (P2 a))
extentAlong sup dir = (minv, maxv)
  where minv = sup (negate dir)
        maxv = sup dir

-- assumes pairs are (min, max)
overlapTest :: (Ord a) => (a, a) -> (a, a) -> Bool
overlapTest (a, b) (c, d) = not (c > b || d < a)

-- intervals are of distance along edge normal of shape X
overlapAmount :: (Ord a, Num a) => (a, a) -> (a, a) -> Maybe a
overlapAmount x@(_, edge) y@(penetrator, _) = toMaybe (overlapTest x y) (edge - penetrator)

unitEdgeNormals :: (Epsilon a, Floating a, Ord a) => VertexView a -> [Feature a (V2 a)]
unitEdgeNormals v = fmap f vs
  where f v' = (v', unitEdgeNormal v')
        vs = vList v

overlapNormal :: Overlap a -> V2 a
overlapNormal = snd . overlapEdge

overlap :: (Floating a, Ord a) => Support a -> Feature a (V2 a) -> Support a -> Maybe (Overlap a)
overlap ss edge@(_, dir) sp = fmap (\oval' -> Overlap { overlapEdge = edge
                                             , overlapDepth = oval'
                                             , overlapPenetrator = penetrator }) oval
  where extentS = extentAlong ss dir
        extentP@(penetrator, _) = extentAlong sp dir
        projectedExtent ex = pairMap f (pairMap snd ex)
                            where f v = dir `afdot'` v
        oval = overlapAmount (projectedExtent extentS) (projectedExtent extentP)

minOverlap :: (Floating a, Ord a) => Support a -> [Feature a (V2 a)] -> Support a -> Maybe (Overlap a)
minOverlap ss edges sp = foldl1 f os
  where os = fmap (\edge -> overlap ss edge sp) edges
        f mino o = do
          mino' <- mino
          o' <- o
          return (if overlapDepth o' < overlapDepth mino' then o' else mino')

minOverlap' :: (Floating a, Ord a) => ShapeInfo a -> ShapeInfo a -> Maybe (Overlap a)
minOverlap' (sa, esa) (sb, _) = minOverlap sa esa sb

contactDepth :: (Floating a) => Feature a (V2 a) -> P2 a -> a
contactDepth (v, n) p = f v' - f p
  where v' = vertex v
        f = afdot' n

contactP2 :: (Floating a) => VertexView a -> ContactP2 a
contactP2 v = (v, vertex v)

penetratingEdge :: (Floating a, Ord a) => Overlap a -> (ContactP2 a, ContactP2 a)
penetratingEdge (Overlap (ve, n) depth b@(vp, bb)) = if bcn < abn then (b, c)
                                                     else (a, b)
  where c@(vc, cc) = contactP2 (vNext vp)
        a@(va, aa) = contactP2 (vPrev vp)
        abn = abs $ (bb .-. aa) `dot` n
        bcn = abs $ (cc .-. bb) `dot` n

penetratedEdge :: (Floating a) => Overlap a -> (ContactP2 a, ContactP2 a)
penetratedEdge (Overlap (ve, _) _ _) = (a, b)
  where a = contactP2 ve
        b = contactP2 (vNext ve)

type ContactP2 a = Feature a (P2 a)
data Contact a = Contact { contactPoints :: Either (ContactP2 a) (ContactP2 a, ContactP2 a)
                         , contactNormal :: V2 a } deriving Show

contactPoints' :: Contact a -> Either (P2 a) (P2 a, P2 a)
contactPoints' = mapBoth f g . contactPoints
  where f = L.view clens
        g = pairMap f

clens :: L.Lens' (ContactP2 a) (P2 a)
clens = L._2

featIndex :: Feature a b -> Int
featIndex = vIndex . fst

flattenContactPoints :: Contact a -> [ContactP2 a]
flattenContactPoints (Contact (Left p) _) = [p]
flattenContactPoints (Contact (Right (p1, p2)) _) = [p1, p2]

clipEdge :: (Floating a, Epsilon a, Ord a) => (ContactP2 a, ContactP2 a) -> V2 a -> (ContactP2 a, ContactP2 a) -> Maybe (Contact a)
clipEdge (aa, bb) n inc_ = do
  inc' <- lApplyClip' l (clipSegment aBound (cd', inc)) inc_
  inc'' <- lApplyClip' l (clipSegment bBound (cd', f inc')) inc'
  contacts <- applyClip'' (clipSegment abBound (cd', f inc'')) inc''
  return Contact { contactPoints = contacts
                  , contactNormal = n }
  where aBound = perpLine2 a b
        bBound = perpLine2 b a
        abBound = Line2 a (-n)
        cd' = toLine2 c d
        inc@(c, d) = f inc_
        (a, b) = f (aa, bb)
        f = pairMap (L.view clens)
        l = clens

-- 'Flipping' indicates the direction of the collision. 'Same' means the first object overlaps into the second.
contact :: (Floating a, Epsilon a, Ord a) => ShapeInfo a -> ShapeInfo a -> Maybe (Flipping (Contact a, Feature a (V2 a)))
contact a b = either (fmap Same . contact_) (fmap Flip . contact_) =<< ovl
  where ovlab = minOverlap' a b
        ovlba = minOverlap' b a
        ovl = maybeBranch (\oab oba -> overlapDepth oab < overlapDepth oba) ovlab ovlba

contact_ :: (Floating a, Epsilon a, Ord a) => Overlap a -> Maybe (Contact a, Feature a (V2 a))
contact_ ovl = fmap f (clipEdge edge n pen)
  where edge = penetratedEdge ovl
        pen = penetratingEdge ovl
        n = overlapNormal ovl
        f c = (c, overlapEdge ovl)

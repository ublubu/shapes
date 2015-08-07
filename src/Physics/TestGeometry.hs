module Physics.TestGeometry where

import Control.Lens
import Linear.Affine
import Linear.V2
import Physics.Geometry
import Physics.Linear
import Physics.Transform

testHull :: ConvexHull Double
testHull = rectangleHull 4 2

testView :: VertexView Double
testView = vertices testHull

testFeature :: Feature Double (WP2 Double)
testFeature = (feat, p)
  where feat = LocalT idTransform testView
        p = WorldT (vertex testView)

testResult :: P2 Double
testResult = view (_2 . wlens) testFeature

testFeature' :: Feature Double (WP2 Double)
testFeature' = set (_2 . wlens) (P $ V2 0 0) testFeature

testPoint :: WP2 Double
testPoint = WorldT (P $ V2 0 0)

testPoint' :: WP2 Double
testPoint' = set wlens (P $ V2 1 0) testPoint

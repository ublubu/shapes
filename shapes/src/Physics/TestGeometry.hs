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

testFeature :: Feature Double (P2 Double)
testFeature = (feat, p)
  where feat = testView
        p = vertex testView

testResult :: P2 Double
testResult = view _2 testFeature

testFeature' :: Feature Double (P2 Double)
testFeature' = set _2 (P $ V2 0 0) testFeature

testPoint :: P2 Double
testPoint = P $ V2 0 0

testPoint' :: P2 Double
testPoint' = P $ V2 1 0

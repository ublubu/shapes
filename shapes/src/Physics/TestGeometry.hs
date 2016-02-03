module Physics.TestGeometry where

import Control.Lens
import Linear.Affine
import Linear.V2
import Physics.ConvexHull
import Physics.Geometry
import Physics.Linear
import Physics.Transform

testVertices :: Vertices Double
testVertices = rectangleVertices 4 2

testHull :: ConvexHull Double
testHull = listToHull testVertices

testPoint :: P2 Double
testPoint = P $ V2 0 0

testPoint' :: P2 Double
testPoint' = P $ V2 1 0

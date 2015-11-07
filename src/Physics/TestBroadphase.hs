module Physics.TestBroadphase where

import Linear.V2
import Physics.Geometry (ConvexHull, rectangleHull, ShapeInfo, shapeInfo)
import Physics.Transform
import Physics.Broadphase

hull0 :: ConvexHull Double
hull0 = transform (translateTransform (V2 0 0 :: V2 Double)) (rectangleHull 2 2)

shape0 :: ShapeInfo Double
shape0 = shapeInfo hull0

aabb0 :: Aabb Double
aabb0 = toAabb shape0

hull1 :: ConvexHull Double
hull1 = transform (translateTransform (V2 1 1 :: V2 Double)) (rectangleHull 2 2)

shape1 :: ShapeInfo Double
shape1 = shapeInfo hull1

aabb1 :: Aabb Double
aabb1 = toAabb shape1


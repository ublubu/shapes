module Physics.Scenes.Rolling where

import Linear.Affine
import Linear.Epsilon
import Linear.V2
import Physics.Constraint
import Physics.Contact
import Physics.External
import Physics.Geometry
import Physics.Object
import Physics.World
import Physics.Scenes.Scene

shapeA :: (Fractional a, Eq a) => PhysicalObj a
shapeA = PhysicalObj { _physObjVel = V2 0 0
                     , _physObjRotVel = 0
                     , _physObjPos = V2 0 (-6)
                     , _physObjRotPos = 0
                     , _physObjHull = ConvexHull [ P $ V2 9 (-0.5)
                                                 , P $ V2 (-9) 10
                                                 , P $ V2 (-9) (-0.5)]
                     , _physObjInvMass = toInvMass2 (0, 0) }

shapeB :: (Fractional a, Eq a) => PhysicalObj a
shapeB = PhysicalObj { _physObjVel = V2 0 0
                     , _physObjRotVel = -3
                     , _physObjPos = V2 (-7) 12
                     , _physObjRotPos = 0
                     , _physObjHull = ConvexHull [ P $ V2 2 1
                                                 , P $ V2 1 2
                                                 , P $ V2 (-1) 2
                                                 , P $ V2 (-2) 1
                                                 , P $ V2 (-2) (-1)
                                                 , P $ V2 (-1) (-2)
                                                 , P $ V2 1 (-2)
                                                 , P $ V2 2 (-1) ]
                     , _physObjInvMass = toInvMass2 (1, 0.5) }

shapeA' :: (Fractional a, Eq a) => WorldObj a
shapeA' = WorldObj shapeA 0.5

shapeB' :: (Fractional a, Eq a) => WorldObj a
shapeB' = WorldObj shapeB 0.5

world :: (Fractional a, Eq a) => World (WorldObj a)
world = fromList [shapeA', shapeB']

externals :: (Physical n a, Epsilon n, Floating n, Ord n) => [External n a]
externals = [constantAccel (V2 0 (-4))]

contactBehavior :: (Floating a) => ContactBehavior a
contactBehavior = ContactBehavior 0.01 0.02

scene :: (Physical a p, Epsilon a, Floating a, Ord a, Eq a) => Scene a p
scene = Scene world externals contactBehavior

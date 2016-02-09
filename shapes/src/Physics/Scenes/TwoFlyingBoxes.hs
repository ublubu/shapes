module Physics.Scenes.TwoFlyingBoxes where

import Linear.Epsilon
import Linear.V2
import Physics.Constraint
import Physics.Contact
import Physics.ConvexHull
import Physics.Object
import Physics.World
import Physics.Scenes.Scene

boxA :: (Fractional a, Eq a) => PhysicalObj a
boxA = PhysicalObj { _physObjVel = V2 1 0
                   , _physObjRotVel = 0
                   , _physObjPos = V2 (-5) 0
                   , _physObjRotPos = 0
                   , _physObjInvMass = toInvMass2 (2, 1) }

boxB :: (Fractional a, Eq a) => PhysicalObj a
boxB = PhysicalObj { _physObjVel = V2 (-4) 0
                   , _physObjRotVel = 0
                   , _physObjPos = V2 5 2
                   , _physObjRotPos = 0
                   , _physObjInvMass = toInvMass2 (1, 0.5) }

boxA' :: (Epsilon a, Floating a, Ord a) => WorldObj a
boxA' = makeWorldObj boxA 0.2 $ rectangleHull 4 4

boxB' :: (Epsilon a, Floating a, Ord a) => WorldObj a
boxB' = makeWorldObj boxB 0.2 $ rectangleHull 2 2

world :: (Epsilon a, Floating a, Ord a) => World (WorldObj a)
world = fromList [boxA', boxB']

externals :: (Physical n a, Epsilon n, Floating n, Ord n) => [External n a]
externals = []

contactBehavior :: (Floating a) => ContactBehavior a
contactBehavior = ContactBehavior 0.01 0.02

scene :: (Physical a p, Epsilon a, Floating a, Ord a, Eq a) => Scene a p
scene = Scene world externals contactBehavior

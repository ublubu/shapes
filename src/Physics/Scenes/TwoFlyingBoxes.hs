module Physics.Scenes.TwoFlyingBoxes where

import Linear.Epsilon
import Linear.V2
import Physics.Constraint
import Physics.Contact
import Physics.External
import Physics.Geometry
import Physics.Object
import Physics.World

boxA :: (Fractional a, Eq a) => PhysicalObj a
boxA = PhysicalObj { _physObjVel = V2 1 0
                   , _physObjRotVel = 0
                   , _physObjPos = V2 (-5) 0
                   , _physObjRotPos = 0
                   , _physObjHull = rectangleHull 4 4
                   , _physObjInvMass = toInvMass2 (2, 1) }

boxB :: (Fractional a, Eq a) => PhysicalObj a
boxB = PhysicalObj { _physObjVel = V2 (-4) 0
                   , _physObjRotVel = 0
                   , _physObjPos = V2 5 2
                   , _physObjRotPos = 0
                   , _physObjHull = rectangleHull 2 2
                   , _physObjInvMass = toInvMass2 (1, 0.5) }

boxA' :: (Fractional a, Eq a) => WorldObj a
boxA' = WorldObj boxA 0.2

boxB' :: (Fractional a, Eq a) => WorldObj a
boxB' = WorldObj boxB 0.2

world :: (Fractional a, Eq a) => World (WorldObj a)
world = fromList [boxA', boxB']

externals :: (Physical n a, Epsilon n, Floating n, Ord n) => [External n a]
externals = []

contactBehavior :: (Floating a) => ContactBehavior a
contactBehavior = ContactBehavior 0.01 0.02

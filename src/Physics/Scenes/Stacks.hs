module Physics.Scenes.Stacks where

import Control.Lens
import Linear.Epsilon
import Linear.V2
import Physics.Constraint
import Physics.Contact
import Physics.External
import Physics.Geometry
import Physics.Object
import Physics.World
import Physics.Scenes.Scene

box2w2h :: (Fractional a, Eq a) => (a, a) -> (a, a) -> PhysicalObj a
box2w2h (x, y) (vx, vy) = PhysicalObj { _physObjVel = V2 vx vy
                   , _physObjRotVel = 0
                   , _physObjPos = V2 x y
                   , _physObjRotPos = 0
                   , _physObjHull = rectangleHull 2 2
                   , _physObjInvMass = toInvMass2 (2, 1) }

boxFloor :: (Fractional a, Eq a) => PhysicalObj a
boxFloor = PhysicalObj { _physObjVel = V2 0 0
                   , _physObjRotVel = 0
                   , _physObjPos = V2 0 (-6)
                   , _physObjRotPos = 0
                   , _physObjHull = rectangleHull 18 1
                   , _physObjInvMass = toInvMass2 (0, 0) }

box2w2h' :: (Epsilon a, Floating a, Ord a) => (a, a) -> (a, a) -> WorldObj a
box2w2h' center velocity = makeWorldObj (box2w2h center velocity) 0.2

boxFloor' :: (Epsilon a, Floating a, Ord a) => WorldObj a
boxFloor' = makeWorldObj boxFloor 0.2

boxStack :: (Epsilon a, Floating a, Ord a) => (a, a) -> (a, a) -> a -> Int -> [WorldObj a]
boxStack _ _ _ 0 = []
boxStack bottom vel spacing n = box2w2h' bottom vel : boxStack bottom' vel spacing (n - 1)
  where bottom' = bottom & _2 %~ (+ (2 + spacing))

world :: (Epsilon a, Floating a, Ord a) => World (WorldObj a)
world = fromList ([boxFloor'] ++ boxStack (8, -4.5) (-1, 0) 0 5 ++ boxStack (5.5, -4.5) (-2, 0) 0 5)

world' :: (Epsilon a, Floating a, Ord a) => World (WorldObj a)
world' = fromList ([boxFloor'] ++ boxStack (0, -4.5) (0, 0) 0 5++ [box2w2h' (8, 0) (-6, 0)])

world'' :: (Epsilon a, Floating a, Ord a) => World (WorldObj a)
world'' = fromList ([boxFloor'] ++ boxStack (0, -4.5) (0, 0) 1 5
                   ++ boxStack (2, -4.5) (0, 0) 1 5
                   ++ boxStack (4, -4.5) (0, 0) 1 5)

externals :: (Physical n a, Epsilon n, Floating n, Ord n) => [External n a]
externals = [constantAccel (V2 0 (-2))]

contactBehavior :: (Floating a) => ContactBehavior a
contactBehavior = ContactBehavior 0.01 0.02

scene :: (Physical a p, Epsilon a, Floating a, Ord a, Eq a) => Scene a p
scene = Scene world externals contactBehavior

scene' :: (Physical a p, Epsilon a, Floating a, Ord a, Eq a) => Scene a p
scene' = Scene world' externals contactBehavior

scene'' :: (Physical a p, Epsilon a, Floating a, Ord a, Eq a) => Scene a p
scene'' = Scene world'' externals contactBehavior

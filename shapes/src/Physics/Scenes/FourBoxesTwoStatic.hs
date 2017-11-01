module Physics.Scenes.FourBoxesTwoStatic where

import Data.Proxy
import Physics.Engine.Class
import Physics.Scenes.Scene
import Physics.World.Class

boxA :: (PhysicsEngine e) => Proxy e -> PEPhysicalObj e
boxA p = makePhysicalObj p (1, 0) 0 (-5, 0) 0 (2, 1)

boxB :: (PhysicsEngine e) => Proxy e -> PEPhysicalObj e
boxB p = makePhysicalObj p (-4, 0) 0 (5, 2) 0 (1, 0.5)

boxC :: (PhysicsEngine e) => Proxy e -> PEPhysicalObj e
boxC p = makePhysicalObj p (0, 0) 0 (0, -6) 0 (0, 0)

boxD :: (PhysicsEngine e) => Proxy e -> PEPhysicalObj e
boxD p = makePhysicalObj p (0, 0) 0 (-5, -4) 0 (1, 0)

staticBoxD :: (PhysicsEngine e) => Proxy e -> PEPhysicalObj e
staticBoxD p = makePhysicalObj p (0, 0) 0 (-5, -4) 0 (0, 0)

boxA' :: (PhysicsEngine e) => Proxy e -> PEWorldObj e
boxA' p = makeWorldObj p (boxA p) 0.2 $ makeRectangleHull p 4 4

boxB' :: (PhysicsEngine e) => Proxy e -> PEWorldObj e
boxB' p = makeWorldObj p (boxB p) 0.2 $ makeRectangleHull p 2 2

boxC' :: (PhysicsEngine e) => Proxy e -> PEWorldObj e
boxC' p = makeWorldObj p (boxC p) 0.2 $ makeRectangleHull p 18 1

boxD' :: (PhysicsEngine e) => Proxy e -> PEWorldObj e
boxD' p = makeWorldObj p (boxD p) 0.2 $ makeRectangleHull p 0.4 3

staticBoxD' :: (PhysicsEngine e) => Proxy e -> PEWorldObj e
staticBoxD' p = makeWorldObj p (staticBoxD p) 0.2 $ makeRectangleHull p 0.4 3

world :: (PhysicsEngine e) => Proxy e -> PEWorld e (PEWorldObj e)
world p = makeWorld p [boxA' p, boxB' p, boxC' p, boxD' p]

world' :: (PhysicsEngine e) => Proxy e -> PEWorld e (PEWorldObj e)
world' p = makeWorld p [boxA' p, boxB' p, boxC' p, staticBoxD' p]

externals :: (PhysicsEngine e) => Proxy e -> [External]
externals p = [makeConstantAccel p (0, -2)]

contactBehavior :: (PhysicsEngine e) => Proxy e -> PEContactBehavior e
contactBehavior p = makeContactBehavior p 0.01 0.02

scene :: (PhysicsEngine e) => Proxy e -> Scene e
scene p = Scene (world p) (externals p) (contactBehavior p)

scene' :: (PhysicsEngine e) => Proxy e -> Scene e
scene' p = Scene (world' p) (externals p) (contactBehavior p)

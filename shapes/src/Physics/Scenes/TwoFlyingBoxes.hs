{-# LANGUAGE FlexibleContexts #-}

module Physics.Scenes.TwoFlyingBoxes where

import Data.Proxy
import Physics.Engine.Class
import Physics.Scenes.Scene
import Physics.World.Class

boxA :: (PhysicsEngine e) => Proxy e -> PEPhysicalObj e
boxA p = makePhysicalObj p (1, 0) 0 (-5, 0) 0 (2, 1)

boxB :: (PhysicsEngine e) => Proxy e -> PEPhysicalObj e
boxB p = makePhysicalObj p (-4, 0) 0 (5, 2) 0 (1, 0.5)

boxA' :: (PhysicsEngine e) => Proxy e -> PEWorldObj e
boxA' p = makeWorldObj p (boxA p) 0.2 $ makeRectangleHull p 4 4

boxB' :: (PhysicsEngine e) => Proxy e -> PEWorldObj e
boxB' p = makeWorldObj p (boxB p) 0.2 $ makeRectangleHull p 2 2

world :: (PhysicsEngine e) => Proxy e -> PEWorld e (PEWorldObj e)
world p = makeWorld p [boxA' p, boxB' p]

externals :: (PhysicsEngine e) => Proxy e -> [External]
externals _ = []

contactBehavior :: (PhysicsEngine e) => Proxy e -> PEContactBehavior e
contactBehavior p = makeContactBehavior p 0.01 0.02

scene :: (PhysicsEngine e) => Proxy e -> Scene e
scene p = Scene (world p) (externals p) (contactBehavior p)

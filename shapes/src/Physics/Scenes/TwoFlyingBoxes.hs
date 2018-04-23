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

boxA' :: (PhysicsEngine e) => Proxy e -> PEExternalObj e -> PEWorldObj' e
boxA' p = makeWorldObj p (boxA p) 0.2 $ makeRectangleHull p 4 4

boxB' :: (PhysicsEngine e) => Proxy e -> PEExternalObj e -> PEWorldObj' e
boxB' p = makeWorldObj p (boxB p) 0.2 $ makeRectangleHull p 2 2

world
  :: (PhysicsEngine e)
  => Proxy e
  -> PEExternalObj e
  -> PEExternalObj e
  -> PEWorld' e
world p a b = makeWorld p [boxA' p a, boxB' p b]

externals :: Proxy e -> [External]
externals _ = []

contactBehavior :: (PhysicsEngine e) => Proxy e -> PEContactBehavior e
contactBehavior p = makeContactBehavior p 0.01 0.02

scene
  :: (PhysicsEngine e)
  => Proxy e
  -> PEExternalObj e
  -> PEExternalObj e
  -> Scene e
scene p a b = Scene (world p a b) (externals p) (contactBehavior p)

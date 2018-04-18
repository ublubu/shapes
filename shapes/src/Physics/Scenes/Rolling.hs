module Physics.Scenes.Rolling where

import Data.Proxy
import Physics.Engine.Class
import Physics.Scenes.Scene
import Physics.World.Class

shapeA :: (PhysicsEngine e) => Proxy e -> PEPhysicalObj e
shapeA p = makePhysicalObj p (0, 0) 0 (0, -6) 0 (0, 0)

shapeB :: (PhysicsEngine e) => Proxy e -> PEPhysicalObj e
shapeB p = makePhysicalObj p (0, 0) (-3) (-7, 12) 0 (1, 0.5)

shapeA' :: (PhysicsEngine e) => Proxy e -> PEExternalObj e -> PEWorldObj' e
shapeA' p = makeWorldObj p (shapeA p) 0.5 $ makeHull p [ (9, -0.5)
                                                       , (-9, 10)
                                                       , (-9, -0.5)
                                                       ]

shapeB' :: (PhysicsEngine e) => Proxy e -> PEExternalObj e -> PEWorldObj' e
shapeB' p = makeWorldObj p (shapeB p) 0.5 $ makeHull p [ (2, 1)
                                                       , (1, 2)
                                                       , (-1, 2)
                                                       , (-2, 1)
                                                       , (-2, -1)
                                                       , (-1, -2)
                                                       , (1, -2)
                                                       , (2, -1)
                                                       ]

world
  :: (PhysicsEngine e)
  => Proxy e
  -> PEExternalObj e
  -> PEExternalObj e
  -> PEWorld' e
world p a b = makeWorld p [shapeA' p a, shapeB' p b]

externals :: (PhysicsEngine e) => Proxy e -> [External]
externals p = [makeConstantAccel p (0, -4)]

contactBehavior :: (PhysicsEngine e) => Proxy e -> PEContactBehavior e
contactBehavior p = makeContactBehavior p 0.01 0.02

scene
  :: (PhysicsEngine e)
  => Proxy e
  -> PEExternalObj e
  -> PEExternalObj e
  -> Scene e
scene p a b = Scene (world p a b) (externals p) (contactBehavior p)

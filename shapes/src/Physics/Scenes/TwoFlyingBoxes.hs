{-# LANGUAGE FlexibleContexts #-}

module Physics.Scenes.TwoFlyingBoxes where

import           Physics.Constraint
import           Physics.Contact.Types
import           Physics.Engine
import           Physics.Scenes.Scene
import           Physics.World
import           Physics.World.Object

boxA :: PhysicalObj
boxA = makePhysicalObj (1, 0) 0 (-5, 0) 0 (2, 1)

boxB :: PhysicalObj
boxB = makePhysicalObj (-4, 0) 0 (5, 2) 0 (1, 0.5)

boxA' :: usr -> WorldObj usr
boxA' = makeWorldObj boxA 0.2 $ makeRectangleHull 4 4

boxB' :: usr -> WorldObj usr
boxB' = makeWorldObj boxB 0.2 $ makeRectangleHull 2 2

world
  :: usr
  -> usr
  -> World usr
world a b = makeWorld [boxA' a, boxB' b]

externals :: [External]
externals = []

contactBehavior :: ContactBehavior
contactBehavior = ContactBehavior 0.01 0.02

scene
  :: usr
  -> usr
  -> Scene usr
scene a b = Scene (world a b) externals contactBehavior

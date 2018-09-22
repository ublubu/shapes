{-# LANGUAGE FlexibleContexts #-}

module Physics.Scenes.TwoFlyingBoxes where

import           Control.Monad.ST
import           Physics.Constraint
import           Physics.Contact.Types
import           Physics.Engine
import           Physics.Scenes.Scene
import           Physics.World

boxA :: PhysicalObj
boxA = makePhysicalObj (1, 0) 0 (-5, 0) 0 (2, 1)

boxB :: PhysicalObj
boxB = makePhysicalObj (-4, 0) 0 (5, 2) 0 (1, 0.5)

boxA' :: label -> WorldObj label
boxA' = makeWorldObj boxA 0.2 0 $ makeRectangleHull 4 4

boxB' :: label -> WorldObj label
boxB' = makeWorldObj boxB 0.2 0 $ makeRectangleHull 2 2

world
  :: label
  -> label
  -> ST s (World s label)
world a b = makeWorld [boxA' a, boxB' b]

externals :: External
externals = const id

contactBehavior :: ContactBehavior
contactBehavior = ContactBehavior 0.01 0.02

scene
  :: label
  -> label
  -> ST s (Scene s label)
scene a b = do
  w <- world a b
  return $ Scene w externals contactBehavior

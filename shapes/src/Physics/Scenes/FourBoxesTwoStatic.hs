module Physics.Scenes.FourBoxesTwoStatic where

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

boxC :: PhysicalObj
boxC = makePhysicalObj (0, 0) 0 (0, -6) 0 (0, 0)

boxD :: PhysicalObj
boxD = makePhysicalObj (0, 0) 0 (-5, -4) 0 (1, 0)

staticBoxD :: PhysicalObj
staticBoxD = makePhysicalObj (0, 0) 0 (-5, -4) 0 (0, 0)

boxA' :: label -> WorldObj label
boxA' = makeWorldObj boxA 0.2 $ makeRectangleHull 4 4

boxB' :: label -> WorldObj label
boxB' = makeWorldObj boxB 0.2 $ makeRectangleHull 2 2

boxC' :: label -> WorldObj label
boxC' = makeWorldObj boxC 0.2 $ makeRectangleHull 18 1

boxD' :: label -> WorldObj label
boxD' = makeWorldObj boxD 0.2 $ makeRectangleHull 0.4 3

staticBoxD' :: label -> WorldObj label
staticBoxD' = makeWorldObj staticBoxD 0.2 $ makeRectangleHull 0.4 3

world
  :: label
  -> label
  -> label
  -> label
  -> ST s (World s label)
world a b c d = makeWorld [boxA' a, boxB' b, boxC' c, boxD' d]

world'
  :: label
  -> label
  -> label
  -> label
  -> ST s (World s label)
world' a b c d = makeWorld [boxA' a, boxB' b, boxC' c, staticBoxD' d]

externals :: External
externals = makeConstantAccel (0, -2)

contactBehavior :: ContactBehavior
contactBehavior = ContactBehavior 0.01 0.02

scene
  :: label
  -> label
  -> label
  -> label
  -> ST s (Scene s label)
scene a b c d = do
  w <- world a b c d
  return $ Scene w externals contactBehavior

scene'
  :: label
  -> label
  -> label
  -> label
  -> ST s (Scene s label)
scene' a b c d = do
  w <- world' a b c d
  return $ Scene w externals contactBehavior

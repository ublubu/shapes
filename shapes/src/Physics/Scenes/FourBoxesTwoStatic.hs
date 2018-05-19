module Physics.Scenes.FourBoxesTwoStatic where

import           Physics.Constraint
import           Physics.Contact.Types
import           Physics.Engine
import           Physics.Scenes.Scene
import           Physics.World
import           Physics.World.Class
import           Physics.World.Object

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

boxA' :: usr -> WorldObj usr
boxA' = makeWorldObj boxA 0.2 $ makeRectangleHull 4 4

boxB' :: usr -> WorldObj usr
boxB' = makeWorldObj boxB 0.2 $ makeRectangleHull 2 2

boxC' :: usr -> WorldObj usr
boxC' = makeWorldObj boxC 0.2 $ makeRectangleHull 18 1

boxD' :: usr -> WorldObj usr
boxD' = makeWorldObj boxD 0.2 $ makeRectangleHull 0.4 3

staticBoxD' :: usr -> WorldObj usr
staticBoxD' = makeWorldObj staticBoxD 0.2 $ makeRectangleHull 0.4 3

world
  :: usr
  -> usr
  -> usr
  -> usr
  -> World (WorldObj usr)
world a b c d = makeWorld [boxA' a, boxB' b, boxC' c, boxD' d]

world'
  :: usr
  -> usr
  -> usr
  -> usr
  -> World (WorldObj usr)
world' a b c d = makeWorld [boxA' a, boxB' b, boxC' c, staticBoxD' d]

externals :: [External]
externals = [makeConstantAccel (0, -2)]

contactBehavior :: ContactBehavior
contactBehavior = ContactBehavior 0.01 0.02

scene
  :: usr
  -> usr
  -> usr
  -> usr
  -> Scene usr
scene a b c d = Scene (world a b c d) externals contactBehavior

scene'
  :: usr
  -> usr
  -> usr
  -> usr
  -> Scene usr
scene' a b c d = Scene (world' a b c d) externals contactBehavior

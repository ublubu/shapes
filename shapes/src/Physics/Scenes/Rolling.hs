module Physics.Scenes.Rolling where

import           Physics.Constraint
import           Physics.Contact.Types
import           Physics.Engine
import           Physics.Scenes.Scene
import           Physics.World
import           Physics.World.Class
import           Physics.World.Object

shapeA :: PhysicalObj
shapeA = makePhysicalObj (0, 0) 0 (0, -6) 0 (0, 0)

shapeB :: PhysicalObj
shapeB = makePhysicalObj (0, 0) (-3) (-7, 12) 0 (1, 0.5)

shapeA' :: usr -> WorldObj usr
shapeA' = makeWorldObj shapeA 0.5 $ makeHull [(9, -0.5), (-9, 10), (-9, -0.5)]

shapeB' :: usr -> WorldObj usr
shapeB' =
  makeWorldObj shapeB 0.5 $
  makeHull
    [(2, 1), (1, 2), (-1, 2), (-2, 1), (-2, -1), (-1, -2), (1, -2), (2, -1)]

world :: usr -> usr -> World (WorldObj usr)
world a b = makeWorld [shapeA' a, shapeB' b]

externals :: [External]
externals = [makeConstantAccel (0, -4)]

contactBehavior :: ContactBehavior
contactBehavior = ContactBehavior 0.01 0.02

scene :: usr -> usr -> Scene usr
scene a b = Scene (world a b) externals contactBehavior

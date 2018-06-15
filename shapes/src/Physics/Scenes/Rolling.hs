module Physics.Scenes.Rolling where

import           Control.Monad.ST
import           Physics.Constraint
import           Physics.Contact.Types
import           Physics.Engine
import           Physics.Scenes.Scene
import           Physics.World

shapeA :: PhysicalObj
shapeA = makePhysicalObj (0, 0) 0 (0, -6) 0 (0, 0)

shapeB :: PhysicalObj
shapeB = makePhysicalObj (0, 0) (-3) (-7, 12) 0 (1, 0.5)

shapeA' :: label -> WorldObj label
shapeA' = makeWorldObj shapeA 0.5 $ makeHull [(9, -0.5), (-9, 10), (-9, -0.5)]

shapeB' :: label -> WorldObj label
shapeB' =
  makeWorldObj shapeB 0.5 $
  makeHull
    [(2, 1), (1, 2), (-1, 2), (-2, 1), (-2, -1), (-1, -2), (1, -2), (2, -1)]

world :: label -> label -> ST s (World s label)
world a b = makeWorld [shapeA' a, shapeB' b]

externals :: External
externals = makeConstantAccel (0, -4)

contactBehavior :: ContactBehavior
contactBehavior = ContactBehavior 0.01 0.02

scene :: label -> label -> ST s (Scene s label)
scene a b = do
  w <- world a b
  return $ Scene w externals contactBehavior

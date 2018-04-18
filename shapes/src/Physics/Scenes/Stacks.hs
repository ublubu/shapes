module Physics.Scenes.Stacks where

import Control.Lens
import Control.Monad
import Data.Proxy
import Physics.Engine.Class
import Physics.Scenes.Scene
import Physics.World.Class

box :: (PhysicsEngine e)
    => Proxy e
    -> (PENumber e, PENumber e)
    -> (PENumber e, PENumber e)
    -> PEPhysicalObj e
box p (x, y) (vx, vy) =
  makePhysicalObj p (vx, vy) 0 (x, y) 0 (2, 1)

boxFloor :: (PhysicsEngine e) => Proxy e -> PEPhysicalObj e
boxFloor p =
  makePhysicalObj p (0, 0) 0 (0, -6) 0 (0, 0)

box' :: (PhysicsEngine e)
     => Proxy e
     -> (PENumber e, PENumber e)
     -> (PENumber e, PENumber e)
     -> (PENumber e, PENumber e)
     -> PEExternalObj e
     -> PEWorldObj' e
box' p (w, h) center velocity =
  makeWorldObj p (box p center velocity) 0.2 (makeRectangleHull p w h)

boxFloor' :: (PhysicsEngine e) => Proxy e -> PEExternalObj e -> PEWorldObj' e
boxFloor' p =
  makeWorldObj p (boxFloor p) 0.2 (makeRectangleHull p 18 1)

boxStack :: (PhysicsEngine e)
         => Proxy e
         -> (PENumber e, PENumber e)
         -> (PENumber e, PENumber e)
         -> (PENumber e, PENumber e)
         -> PENumber e
         -> Int
         -> PEExternalObj e
         -> [PEWorldObj' e]
boxStack _ _ _ _ _ 0 _ = []
boxStack p size@(_, h) bottom vel spacing n ext =
  box' p size bottom vel ext : boxStack p size bottom' vel spacing (n - 1) ext
  where bottom' = bottom & _2 %~ (+ (h + spacing))

stacks :: (PhysicsEngine e)
       => Proxy e
       -> (PENumber e, PENumber e)
       -> (PENumber e, PENumber e)
       -> (PENumber e, PENumber e)
       -> PENumber e
       -> (Int, Int)
       -> PEExternalObj e
       -> [PEWorldObj' e]
stacks p size@(w, _) (center, bottom) vel spacing (n_w, n_h) ext =
  join . fmap f . take n_w $ iterate (+ w) leftmost
  where leftmost = center - (w * fromIntegral (n_w - 1) / 2)
        f left = boxStack p size (left, bottom) vel spacing n_h ext

world :: (PhysicsEngine e) => Proxy e -> PEExternalObj e -> PEWorld' e
world p ext = makeWorld p $ concat [ [boxFloor' p ext]
                                   , boxStack p (2, 2) (8  , -4.5) (-1, 0) 0 5 ext
                                   , boxStack p (2, 2) (5.5, -4.5) (-2, 0) 0 5 ext
                                   ]

world' :: (PhysicsEngine e) => Proxy e -> PEExternalObj e -> PEWorld' e
world' p ext = makeWorld p $ concat [ [boxFloor' p ext]
                                    , boxStack p (2, 2) (0, -4.5) (0, 0) 0 5 ext
                                    , [box' p (2, 2) (8, 0) (-6, 0) ext]
                                    ]

world'' :: (PhysicsEngine e) => Proxy e -> PEExternalObj e -> PEWorld' e
world'' p ext =
  makeWorld p (boxFloor' p ext : stacks p (1, 1) (0, -4.5) (0, 0) 1 (10, 10) ext)

world''' :: (PhysicsEngine e) => Proxy e -> PEExternalObj e -> PEWorld' e
world''' p ext =
  makeWorld p (boxFloor' p ext : stacks p (0.75, 0.75) (0, -4.5) (0, 0) 1 (15, 15) ext)

externals :: (PhysicsEngine e) => Proxy e -> [External]
externals p = [makeConstantAccel p (0, -2)]

contactBehavior :: (PhysicsEngine e) => Proxy e -> PEContactBehavior e
contactBehavior p = makeContactBehavior p 0.01 0.02

scene :: (PhysicsEngine e) => Proxy e -> PEExternalObj e -> Scene e
scene p ext = Scene (world p ext) (externals p) (contactBehavior p)

scene' :: (PhysicsEngine e) => Proxy e -> PEExternalObj e -> Scene e
scene' p ext = Scene (world' p ext) (externals p) (contactBehavior p)

scene'' :: (PhysicsEngine e) => Proxy e -> PEExternalObj e -> Scene e
scene'' p ext = Scene (world'' p ext) (externals p) (contactBehavior p)

scene''' :: (PhysicsEngine e) => Proxy e -> PEExternalObj e -> Scene e
scene''' p ext = Scene (world''' p ext) (externals p) (contactBehavior p)

makeScene :: (PhysicsEngine e) => (Int, Int) -> PENumber e -> Proxy e -> PEExternalObj e -> Scene e
makeScene dims spacing p ext = Scene w (externals p) (contactBehavior p)
  where w = makeWorld p (boxFloor' p ext : stacks p (0.2, 0.2) (0, -4.5) (0, 0) spacing dims ext)

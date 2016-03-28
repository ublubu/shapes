module Physics.Scenes.Stacks where

import Control.Lens
import Control.Monad
import Data.Proxy
import Physics.Engine.Class
import Physics.Scenes.Scene

--box :: (Fractional a, Eq a) => (a, a) -> (a, a) -> (a, a) -> PhysicalObj a
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

--box' :: (Epsilon a, Floating a, Ord a) => (a, a) -> (a, a) -> (a, a) -> WorldObj a
box' :: (PhysicsEngine e)
     => Proxy e
     -> (PENumber e, PENumber e)
     -> (PENumber e, PENumber e)
     -> (PENumber e, PENumber e)
     -> PEWorldObj e
box' p (w, h) center velocity =
  makeWorldObj p (box p center velocity) 0.2 (makeRectangleHull p w h)

--boxFloor' :: (Epsilon a, Floating a, Ord a) => WorldObj a
boxFloor' :: (PhysicsEngine e) => Proxy e -> PEWorldObj e
boxFloor' p =
  makeWorldObj p (boxFloor p) 0.2 (makeRectangleHull p 18 1)

--boxStack :: (Epsilon a, Floating a, Ord a) => (a, a) -> (a, a) -> (a, a) -> a -> Int -> [WorldObj a]
boxStack :: (PhysicsEngine e)
         => Proxy e
         -> (PENumber e, PENumber e)
         -> (PENumber e, PENumber e)
         -> (PENumber e, PENumber e)
         -> PENumber e
         -> Int
         -> [PEWorldObj e]
boxStack _ _ _ _ _ 0 = []
boxStack p size@(_, h) bottom vel spacing n =
  box' p size bottom vel : boxStack p size bottom' vel spacing (n - 1)
  where bottom' = bottom & _2 %~ (+ (h + spacing))

stacks :: (PhysicsEngine e)
       => Proxy e
       -> (PENumber e, PENumber e)
       -> (PENumber e, PENumber e)
       -> (PENumber e, PENumber e)
       -> PENumber e
       -> (Int, Int)
       -> [PEWorldObj e]
stacks p size@(w, _) (center, bottom) vel spacing (n_w, n_h) =
  join . fmap f . take n_w $ iterate (+ w) leftmost
  where leftmost = center - (w * fromIntegral (n_w - 1) / 2)
        f left = boxStack p size (left, bottom) vel spacing n_h

world :: (PhysicsEngine e) => Proxy e -> PEWorld e (PEWorldObj e)
world p = makeWorld p ([boxFloor' p] ++ boxStack p (2, 2) (8, -4.5) (-1, 0) 0 5 ++ boxStack p (2, 2) (5.5, -4.5) (-2, 0) 0 5)

world' :: (PhysicsEngine e) => Proxy e -> PEWorld e (PEWorldObj e)
world' p = makeWorld p ([boxFloor' p] ++ boxStack p (2, 2) (0, -4.5) (0, 0) 0 5 ++ [box' p (2, 2) (8, 0) (-6, 0)])

world'' :: (PhysicsEngine e) => Proxy e -> PEWorld e (PEWorldObj e)
world'' p =
  makeWorld p (boxFloor' p : stacks p (1, 1) (0, -4.5) (0, 0) 1 (10, 10))

world''' :: (PhysicsEngine e) => Proxy e -> PEWorld e (PEWorldObj e)
world''' p =
  makeWorld p (boxFloor' p : stacks p (0.75, 0.75) (0, -4.5) (0, 0) 1 (15, 15))

externals :: (PhysicsEngine e) => Proxy e -> [PEExternal' e]
externals p = [makeConstantAccel p (0, -2)]

contactBehavior :: (PhysicsEngine e) => Proxy e -> PEContactBehavior e
contactBehavior p = makeContactBehavior p 0.01 0.02

scene :: (PhysicsEngine e) => Proxy e -> Scene e
scene p = Scene (world p) (externals p) (contactBehavior p)

scene' :: (PhysicsEngine e) => Proxy e -> Scene e
scene' p = Scene (world' p) (externals p) (contactBehavior p)

scene'' :: (PhysicsEngine e) => Proxy e -> Scene e
scene'' p = Scene (world'' p) (externals p) (contactBehavior p)

scene''' :: (PhysicsEngine e) => Proxy e -> Scene e
scene''' p = Scene (world''' p) (externals p) (contactBehavior p)

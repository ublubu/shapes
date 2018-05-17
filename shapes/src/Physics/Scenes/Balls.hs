module Physics.Scenes.Balls where

import Control.Lens
import Control.Monad
import Data.Proxy
import Physics.Engine.Class
import Physics.Scenes.Scene
import Physics.World.Class
import Physics.Scenes.Stacks (box, boxStack, boxFloor', contactBehavior, externals)
import Physics.Scenes.TwoFlyingBoxes (boxB')

circle' :: (PhysicsEngine e)
     => Proxy e
     -> PENumber e
     -> (PENumber e, PENumber e)
     -> (PENumber e, PENumber e)
     -> PEExternalObj e
     -> PEWorldObj' e
circle' p radius center velocity =
  makeWorldObj p (box p center velocity) 0.2 (makeCircle p radius)

circleStack :: (PhysicsEngine e)
         => Proxy e
         -> PENumber e -- ^ radius
         -> (PENumber e, PENumber e) -- ^ bottom position
         -> (PENumber e, PENumber e) -- ^ velocity
         -> PENumber e -- ^ vertical spacing
         -> Int -- ^ number of objects
         -> PEExternalObj e -- ^ arbitrary user data
         -> [PEWorldObj' e]
circleStack _ _ _ _ _ 0 _ = []
circleStack p diameter bottom vel spacing n ext =
  circle' p (diameter / 2) bottom vel ext : circleStack p diameter bottom' vel spacing (n - 1) ext
  where bottom' = bottom & _2 %~ (+ (diameter + spacing))

stacks_ :: (PhysicsEngine e)
       => (Bool -> Bool)
       -> Proxy e
       -> PENumber e
       -> (PENumber e, PENumber e)
       -> (PENumber e, PENumber e)
       -> PENumber e
       -> (Int, Int)
       -> PEExternalObj e
       -> [PEWorldObj' e]
stacks_ ftype p diameter (center, bottom) vel spacing (n_w, n_h) ext =
  join . fmap f . take n_w $ iterate (\(a, b) -> (a + diameter, ftype b)) (leftmost, True)
  where leftmost = center - (diameter * fromIntegral (n_w - 1) / 2)
        f (left, True) = circleStack p diameter (left, bottom) vel spacing n_h ext
        f (left, False) = boxStack p (diameter, diameter) (left, bottom) vel spacing n_h ext

stacks :: (PhysicsEngine e)
       => Proxy e
       -> PENumber e
       -> (PENumber e, PENumber e)
       -> (PENumber e, PENumber e)
       -> PENumber e
       -> (Int, Int)
       -> PEExternalObj e
       -> [PEWorldObj' e]
stacks = stacks_ not

stacks' :: (PhysicsEngine e)
       => Proxy e
       -> PENumber e
       -> (PENumber e, PENumber e)
       -> (PENumber e, PENumber e)
       -> PENumber e
       -> (Int, Int)
       -> PEExternalObj e
       -> [PEWorldObj' e]
stacks' = stacks_ (const True)

makeScene :: (PhysicsEngine e) => (Int, Int) -> PENumber e -> PENumber e -> Proxy e -> PEExternalObj e -> Scene e
makeScene dims diameter spacing p ext = Scene w (externals p) (contactBehavior p)
  where w = makeWorld p (boxFloor' p ext : stacks p diameter (0, -4.5) (0, 0) spacing dims ext)

makeScene' :: (PhysicsEngine e) => (Int, Int) -> PENumber e -> PENumber e -> Proxy e -> PEExternalObj e -> Scene e
makeScene' dims diameter spacing p ext = Scene w (externals p) (contactBehavior p)
  where w = makeWorld p (boxFloor' p ext : stacks' p diameter (0, -4.5) (0, 0) spacing dims ext)

circleA :: (PhysicsEngine e) => Proxy e -> PEPhysicalObj e
circleA p = makePhysicalObj p (1, 0) 0 (-5, 0) 0 (2, 1)

circleB :: (PhysicsEngine e) => Proxy e -> PEPhysicalObj e
circleB p = makePhysicalObj p (-4, 0) 0 (5, 1.5) 0 (1, 0.5)

circleA' :: (PhysicsEngine e) => Proxy e -> PEExternalObj e -> PEWorldObj' e
circleA' p = makeWorldObj p (circleA p) 0.2 $ makeCircle p 2

circleB' :: (PhysicsEngine e) => Proxy e -> PEExternalObj e -> PEWorldObj' e
circleB' p = makeWorldObj p (circleB p) 0.2 $ makeCircle p 1

twoCircles :: (PhysicsEngine e) => Proxy e -> PEExternalObj e -> PEExternalObj e -> Scene e
twoCircles p a b = Scene world [] (contactBehavior p)
  where world = makeWorld p [circleA' p a, circleB' p b]

circleAndBox :: (PhysicsEngine e) => Proxy e -> PEExternalObj e -> PEExternalObj e -> Scene e
circleAndBox p a b = Scene world [] (contactBehavior p)
  where world = makeWorld p [circleA' p a, boxB' p b]

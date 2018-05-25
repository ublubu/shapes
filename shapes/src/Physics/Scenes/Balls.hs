module Physics.Scenes.Balls where

import           Control.Lens
import           Control.Monad

import           Physics.Constraint
import           Physics.Contact.Types
import           Physics.Engine
import           Physics.Scenes.Scene
import           Physics.Scenes.Stacks         (box, boxFloor', boxStack,
                                                contactBehavior, externals)
import           Physics.Scenes.TwoFlyingBoxes (boxB')
import           Physics.World
import           Physics.World.Object

circle' :: Double
     -> (Double, Double)
     -> (Double, Double)
     -> usr
     -> WorldObj usr
circle' radius center velocity =
  makeWorldObj (box center velocity) 0.2 (makeCircle radius)

circleStack :: Double -- ^ radius
         -> (Double, Double) -- ^ bottom position
         -> (Double, Double) -- ^ velocity
         -> Double -- ^ vertical spacing
         -> Int -- ^ number of objects
         -> usr -- ^ arbitrary user data
         -> [WorldObj usr]
circleStack _ _ _ _ 0 _ = []
circleStack diameter bottom vel spacing n ext =
  circle' (diameter / 2) bottom vel ext : circleStack diameter bottom' vel spacing (n - 1) ext
  where bottom' = bottom & _2 %~ (+ (diameter + spacing))

stacks_ :: (Bool -> Bool)
       -> Double
       -> (Double, Double)
       -> (Double, Double)
       -> Double
       -> (Int, Int)
       -> usr
       -> [WorldObj usr]
stacks_ ftype diameter (center, bottom) vel spacing (n_w, n_h) ext =
  join . fmap f . take n_w $ iterate (\(a, b) -> (a + diameter, ftype b)) (leftmost, True)
  where leftmost = center - (diameter * fromIntegral (n_w - 1) / 2)
        f (left, True) = circleStack diameter (left, bottom) vel spacing n_h ext
        f (left, False) = boxStack (diameter, diameter) (left, bottom) vel spacing n_h ext

stacks :: Double
       -> (Double, Double)
       -> (Double, Double)
       -> Double
       -> (Int, Int)
       -> usr
       -> [WorldObj usr]
stacks = stacks_ not

stacks' :: Double
       -> (Double, Double)
       -> (Double, Double)
       -> Double
       -> (Int, Int)
       -> usr
       -> [WorldObj usr]
stacks' = stacks_ (const True)

makeScene :: (Int, Int) -> Double -> Double -> usr -> Scene usr
makeScene dims diameter spacing ext = Scene w externals contactBehavior
  where w = makeWorld (boxFloor' ext : stacks diameter (0, -4.5) (0, 0) spacing dims ext)

makeScene' :: (Int, Int) -> Double -> Double -> usr -> Scene usr
makeScene' dims diameter spacing ext = Scene w externals contactBehavior
  where w = makeWorld (boxFloor' ext : stacks' diameter (0, -4.5) (0, 0) spacing dims ext)

circleA :: PhysicalObj
circleA = makePhysicalObj (1, 0) 0 (-5, 0) 0 (2, 1)

circleB :: PhysicalObj
circleB = makePhysicalObj (-4, 0) 0 (5, 1.5) 0 (1, 0.5)

circleA' :: usr -> WorldObj usr
circleA' = makeWorldObj circleA 0.2 $ makeCircle 2

circleB' :: usr -> WorldObj usr
circleB' = makeWorldObj circleB 0.2 $ makeCircle 1

twoCircles :: usr -> usr -> Scene usr
twoCircles a b = Scene world [] contactBehavior
  where world = makeWorld [circleA' a, circleB' b]

circleAndBox :: usr -> usr -> Scene usr
circleAndBox a b = Scene world [] contactBehavior
  where world = makeWorld [circleA' a, boxB' b]
